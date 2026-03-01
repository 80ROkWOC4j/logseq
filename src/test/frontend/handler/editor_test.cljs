(ns frontend.handler.editor-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [frontend.commands :as commands]
            [frontend.db :as db]
            [frontend.db.model :as model]
            [frontend.handler.editor :as editor]
            [frontend.handler.notification :as notification]
            [frontend.state :as state]
            [frontend.test.helper :as test-helper]
            [frontend.util :as util]
            [frontend.util.cursor :as cursor]
            [goog.dom :as gdom]
            [goog.object :as gobj]))

(use-fixtures :each test-helper/start-and-destroy-db)

(defn- sync-cursor!
  [pos* input]
  (let [pos @pos*]
    (set! (.-selectionStart input) pos)
    (set! (.-selectionEnd input) pos)))

(defn- insert-text-at-cursor!
  [value* pos* input text]
  (let [pos @pos*
        before (subs @value* 0 pos)
        after (subs @value* pos)
        next-value (str before text after)]
    (reset! value* next-value)
    (set! (.-value input) next-value)
    (swap! pos* + (count text))
    (sync-cursor! pos* input)))

(defn- capture-autopair-side-effects!
  [steps* warnings* action-data* pos selected prefix]
  (cond
    (= prefix "[[")
    (do
      (swap! steps* conj [:editor/search-page])
      (reset! action-data* {:pos {:pos pos}
                            :selected selected}))

    (= prefix "((")
    (do
      (swap! steps* conj [:editor/search-block :reference])
      (swap! warnings* conj [:warning])
      (reset! action-data* {:pos {:pos pos}
                            :selected selected}))

    :else
    nil))

(defn- keydown-not-matched-handler
  [{:keys [value cursor-pos event events input-char strict-composing? process-composing?]
    :or {value ""
         cursor-pos 0
         strict-composing? false
         process-composing? false}}]
  (let [calls (atom [])
        steps* (atom [])
        warnings* (atom [])
        action-data* (atom nil)
        stopped? (atom false)
        value* (atom value)
        pos* (atom cursor-pos)
        events* (or events (when event [event]))
        input (js-obj "value" value
                      "selectionStart" cursor-pos
                      "selectionEnd" cursor-pos
                      "getBoundingClientRect" (fn []
                                                #js {:toJSON (fn []
                                                               #js {:left 0
                                                                    :top 0
                                                                    :width 0
                                                                    :height 0})}))]
    (gobj/set input "setSelectionRange"
              (fn [start end]
                (set! (.-selectionStart input) start)
                (set! (.-selectionEnd input) end)))
    (state/set-editor-action! nil)
    (with-redefs [state/get-edit-input-id (constantly "id")
                  state/get-input (constantly input)
                  gdom/getElement (constantly input)
                  cursor/pos (fn [_] @pos*)
                  cursor/move-cursor-forward (fn [_]
                                               (swap! pos* inc)
                                               (sync-cursor! pos* input))
                  cursor/get-caret-pos (fn [_] {:pos @pos*})
                  util/get-selected-text (constantly "")
                  state/get-editor-show-page-search-hashtag? (constantly false)
                  util/goog-event-is-composing?
                  (fn [_e include-process?]
                    (if include-process?
                      process-composing?
                      strict-composing?))
                  commands/simple-insert!
                  (fn [_id text {:keys [backward-pos]}]
                    (insert-text-at-cursor! value* pos* input text)
                    (swap! pos* - (or backward-pos 0))
                    (sync-cursor! pos* input))
                  util/stop (fn [_] (reset! stopped? true))
                  editor/autopair
                  (fn [input-id key format option]
                    (swap! calls conj [input-id key format option])
                    (let [close-char (get editor/autopair-map key)
                          pos @pos*
                          before (subs @value* 0 pos)
                          after (subs @value* pos)
                          next-value (str before key close-char after)]
                      (reset! value* next-value)
                      (set! (.-value input) next-value)
                      (swap! pos* inc)
                      (sync-cursor! pos* input)
                      (when-let [prefix (#'editor/get-autopair-side-effect-prefix next-value (- @pos* 2))]
                        (capture-autopair-side-effects! steps*
                                                        warnings*
                                                        action-data*
                                                        @pos*
                                                        ""
                                                        prefix))))]
      (doseq [event events*]
        (reset! stopped? false)
        (let [calls-before (count @calls)]
          ((editor/keydown-not-matched-handler :markdown) event nil)
          (when (and (= calls-before (count @calls))
                     (string? input-char)
                     (not @stopped?))
            (insert-text-at-cursor! value* pos* input input-char))
          ;; Keep repeat tests focused on autopair behavior, not popup state retention.
          (state/clear-editor-action!)))
      {:value @value*
       :pos @pos*
       :steps @steps*
       :warnings @warnings*
       :action-data @action-data*})))

(defn- ime-composition-end-handler
  [{:keys [value cursor-pos input-char repeat-count pre-commit?]
    :or {value ""
         cursor-pos 0
         repeat-count 1
         pre-commit? true}}]
  (let [steps* (atom [])
        warnings* (atom [])
        action-data* (atom nil)
        value* (atom value)
        pos* (atom cursor-pos)
        input (js-obj "value" value
                      "selectionStart" cursor-pos
                      "selectionEnd" cursor-pos
                      "getBoundingClientRect" (fn []
                                                #js {:toJSON (fn []
                                                               #js {:left 0
                                                                    :top 0
                                                                    :width 0
                                                                    :height 0})}))]
    (gobj/set input "setSelectionRange"
              (fn [start end]
                (set! (.-selectionStart input) start)
                (set! (.-selectionEnd input) end)))
    (state/set-editor-action! nil)
    (with-redefs [state/get-edit-input-id (constantly "id")
                  state/get-input (constantly input)
                  editor/ime-composition-start-context (atom {})
                  gdom/getElement (constantly input)
                  cursor/pos (fn [_] @pos*)
                  cursor/get-caret-pos (fn [_] {:pos @pos*})
                  cursor/move-cursor-to (fn
                                          ([_input n]
                                           (reset! pos* n)
                                           (sync-cursor! pos* input))
                                          ([_input n _delay?]
                                           (reset! pos* n)
                                           (sync-cursor! pos* input)))
                  util/get-selected-text (constantly "")
                  state/set-block-content-and-last-pos!
                  (fn [_id value pos]
                    (reset! value* value)
                    (set! (.-value input) value)
                    (reset! pos* pos)
                    (sync-cursor! pos* input))
                  commands/handle-step (fn [step] (swap! steps* conj step))
                  notification/show! (fn [_message level]
                                       (swap! warnings* conj [level]))
                  state/set-editor-action-data! (fn [m] (reset! action-data* m))
                  editor/schedule-ime-autopair! (fn [f] (f))
                  commands/simple-insert!
                  (fn [_id text {:keys [backward-pos]}]
                    (insert-text-at-cursor! value* pos* input text)
                    (swap! pos* - (or backward-pos 0))
                    (sync-cursor! pos* input))]
      (dotimes [_ repeat-count]
        (#'editor/ime-composition-autopair! #js {:type "compositionstart"
                                                 :data ""}
         "id")
        ;; Some IMEs commit the opener before compositionend, some do it after.
        (when pre-commit?
          (insert-text-at-cursor! value* pos* input input-char))
        (#'editor/ime-composition-autopair! #js {:type "compositionend"
                                                 :data input-char}
         "id")
        ;; Keep repeat tests focused on autopair behavior, not popup state retention.
        (state/clear-editor-action!))
      {:value @value*
       :pos @pos*
       :steps @steps*
       :warnings @warnings*
       :action-data @action-data*})))

(defn- backspace-delete-pair-handler
  [{:keys [value cursor-pos]
    :or {value ""
         cursor-pos 0}}]
  (let [value* (atom value)
        pos* (atom cursor-pos)
        input #js {:id "id" :value value}
        original-document (gobj/get js/globalThis "document")
        fake-document #js {:activeElement input}]
    (gobj/set js/globalThis "document" fake-document)
    (try
      (with-redefs [state/get-input (constantly input)
                    state/get-edit-input-id (constantly "id")
                    cursor/pos (fn [_] @pos*)
                    util/get-selection-start (fn [_] @pos*)
                    util/get-selection-end (fn [_] @pos*)
                    state/get-current-repo (constantly nil)
                    util/stop (fn [_] nil)
                    state/get-editor-show-page-search? (constantly false)
                    state/get-editor-show-block-search? (constantly false)
                    commands/delete-pair!
                    (fn [_id]
                      (let [pos @pos*
                            prefix (subs @value* 0 (dec pos))
                            next-value (str prefix (subs @value* (inc pos)))]
                        (reset! value* next-value)
                        (set! (.-value input) next-value)
                        (reset! pos* (count prefix))))]
        (editor/keydown-backspace-handler false #js {})
        {:value @value*
         :pos @pos*})
      (finally
        (gobj/set js/globalThis "document" original-document)))))

(def ime-autopair-cases
  [["(" "(" "Digit9" true]
   ["{" "{" "BracketLeft" true]
   ["`" "`" "Backquote" false]
   ["[" "[" "BracketLeft" false]])

(defn- expected-paired-value
  [input-char repeat-count]
  (let [close-char (get editor/autopair-map input-char)]
    (str (apply str (repeat repeat-count input-char))
         (apply str (repeat repeat-count close-char)))))

(defn- assert-ime-composition-autopair!
  [repeat-count]
  (doseq [[label english-key ime-code ime-shift] ime-autopair-cases]
    (testing (str "Autopair should work in both english and IME modes for " label
                  " repeated " repeat-count " times")
      (let [expected (expected-paired-value label repeat-count)
            english-event #js {:key english-key
                               :code ime-code
                               :shiftKey ime-shift}
            english-result (keydown-not-matched-handler
                            {:events (vec (repeat repeat-count english-event))
                             :input-char label})
            ime-result (ime-composition-end-handler
                        {:input-char label
                         :repeat-count repeat-count})
            english-detail (str "repeat-count=" repeat-count
                                "\nchar=" label
                                "\nmode=english"
                                "\nexpected-final=" expected
                                "\nactual-final=" (pr-str (:value english-result)))
            ime-detail (str "repeat-count=" repeat-count
                            "\nchar=" label
                            "\nmode=ime"
                            "\nexpected-final=" expected
                            "\nactual-final=" (pr-str (:value ime-result)))]
        (is (= expected (:value english-result))
            (str "English final editor value should be paired for " label "\n" english-detail))
        (is (= expected (:value ime-result))
            (str "IME final editor value should be paired for " label "\n" ime-detail))))))

(deftest ime-composition-autopair-test
  (doseq [repeat-count [1 2 3]]
    (assert-ime-composition-autopair! repeat-count)))

(deftest ime-composition-autopair-no-precommit-fallback-test
  (doseq [[label english-key ime-code ime-shift] ime-autopair-cases]
    (testing (str "Autopair should still work when compositionend happens before opener commit for " label)
      (let [expected (expected-paired-value label 1)
            english-event #js {:key english-key
                               :code ime-code
                               :shiftKey ime-shift}
            english-result (keydown-not-matched-handler
                            {:events [english-event]
                             :input-char label})
            ime-result (ime-composition-end-handler
                        {:input-char label
                         :repeat-count 1
                         :pre-commit? false})
            detail (str "char=" label
                        "\nexpected-final=" expected
                        "\nenglish-final=" (pr-str (:value english-result))
                        "\nime-final=" (pr-str (:value ime-result)))]
        (is (= expected (:value english-result))
            (str "English final editor value should be paired for " label "\n" detail))
        (is (= expected (:value ime-result))
            (str "IME final editor value should be paired for " label "\n" detail))))))

(deftest ime-composition-tilde-forward-parity-test
  (doseq [[repeat-count expected] [[1 "~~"]
                                   [2 "~~"]
                                   [3 "~~~~"]]]
    (testing (str "Tilde autopair should match english forward behavior, repeat-count=" repeat-count)
      (let [events (vec (repeat repeat-count #js {:key "~"
                                                  :code "Backquote"
                                                  :shiftKey true}))
            english (keydown-not-matched-handler {:events events
                                                  :input-char "~"})
            ime (ime-composition-end-handler {:input-char "~"
                                              :repeat-count repeat-count})]
        (is (= expected (:value english)))
        (is (= expected (:value ime)))
        (is (= (:value english) (:value ime)))))))

(deftest ime-composition-autopair-backspace-delete-pair-test
  (doseq [repeat-count [1 2 3]
          [label english-key ime-code ime-shift] ime-autopair-cases]
    (testing (str "Backspace should delete paired chars in both english and IME modes for "
                  label " repeated " repeat-count " times")
      (let [expected-before (expected-paired-value label repeat-count)
            expected-after (if (= repeat-count 1)
                             ""
                             (expected-paired-value label (dec repeat-count)))
            english-event #js {:key english-key
                               :code ime-code
                               :shiftKey ime-shift}
            english-typed (keydown-not-matched-handler
                           {:events (vec (repeat repeat-count english-event))
                            :input-char label})
            english-after-backspace (backspace-delete-pair-handler
                                     {:value (:value english-typed)
                                      :cursor-pos (:pos english-typed)})
            ime-typed (ime-composition-end-handler
                       {:input-char label
                        :repeat-count repeat-count})
            ime-after-backspace (backspace-delete-pair-handler
                                 {:value (:value ime-typed)
                                  :cursor-pos (:pos ime-typed)})
            english-detail (str "repeat-count=" repeat-count
                                "\nchar=" label
                                "\nmode=english"
                                "\nexpected-before=" expected-before
                                "\nactual-before=" (pr-str (:value english-typed))
                                "\nexpected-after=" expected-after
                                "\nactual-after=" (pr-str (:value english-after-backspace)))
            ime-detail (str "repeat-count=" repeat-count
                            "\nchar=" label
                            "\nmode=ime"
                            "\nexpected-before=" expected-before
                            "\nactual-before=" (pr-str (:value ime-typed))
                            "\nexpected-after=" expected-after
                            "\nactual-after=" (pr-str (:value ime-after-backspace)))]
        (is (= expected-before (:value english-typed))
            (str "English precondition should be paired before backspace for " label "\n" english-detail))
        (is (= expected-before (:value ime-typed))
            (str "IME precondition should be paired before backspace for " label "\n" ime-detail))
        (is (= expected-after (:value english-after-backspace))
            (str "English backspace should delete one pair for " label "\n" english-detail))
        (is (= expected-after (:value ime-after-backspace))
            (str "IME backspace should delete one pair for " label "\n" ime-detail))))))

(deftest ime-composition-autopair-side-effects-test
  (testing "[[ side effects should match between english and IME"
    (let [english (keydown-not-matched-handler
                   {:events [#js {:key "[" :code "BracketLeft" :shiftKey false}
                             #js {:key "[" :code "BracketLeft" :shiftKey false}]
                    :input-char "["})
          ime (ime-composition-end-handler {:input-char "[" :repeat-count 2})]
      (is (= (:steps english) (:steps ime)))
      (is (= (:warnings english) (:warnings ime)))
      (is (map? (:action-data english)))
      (is (map? (:action-data ime)))))

  (testing "(( side effects should match between english and IME"
    (let [english (keydown-not-matched-handler
                   {:events [#js {:key "(" :code "Digit9" :shiftKey true}
                             #js {:key "(" :code "Digit9" :shiftKey true}]
                    :input-char "("})
          ime (ime-composition-end-handler {:input-char "(" :repeat-count 2})]
      (is (= (:steps english) (:steps ime)))
      (is (= (:warnings english) (:warnings ime)))
      (is (= [[:editor/search-block :reference]] (:steps ime)))
      (is (= 1 (count (:warnings english))))
      (is (= 1 (count (:warnings ime))))
      (is (= [:warning] (first (:warnings english))))
      (is (= [:warning] (first (:warnings ime))))
      (is (map? (:action-data english)))
      (is (map? (:action-data ime)))))

  (testing "{{ should have no page/block side effects"
    (let [english (keydown-not-matched-handler
                   {:events [#js {:key "{" :code "BracketLeft" :shiftKey true}
                             #js {:key "{" :code "BracketLeft" :shiftKey true}]
                    :input-char "{"})
          ime (ime-composition-end-handler {:input-char "{" :repeat-count 2})]
      (is (empty? (:steps english)))
      (is (empty? (:steps ime)))
      (is (empty? (:warnings english)))
      (is (empty? (:warnings ime))))))

(deftest extract-nearest-link-from-text-test
  (testing "Page, block and tag links"
    (is (= "page1"
           (editor/extract-nearest-link-from-text "[[page1]] [[page2]]" 0))
        "Finds first page link correctly based on cursor position")

    (is (= "page2"
           (editor/extract-nearest-link-from-text "[[page1]] [[page2]]" 10))
        "Finds second page link correctly based on cursor position")

    (is (= "tag"
           (editor/extract-nearest-link-from-text "#tag [[page1]]" 3))
        "Finds tag correctly")

    (is (= "61e057b9-f799-4532-9258-cfef6ce58370"
           (editor/extract-nearest-link-from-text
            "((61e057b9-f799-4532-9258-cfef6ce58370)) #todo" 5))
        "Finds block correctly"))

  (testing "Url links"
    (is (= "https://github.com/logseq/logseq"
           (editor/extract-nearest-link-from-text
            "https://github.com/logseq/logseq is #awesome :)" 0 editor/url-regex))
        "Finds url correctly")

    (is (not= "https://github.com/logseq/logseq"
              (editor/extract-nearest-link-from-text
               "https://github.com/logseq/logseq is #awesome :)" 0))
        "Doesn't find url if regex not passed")

    (is (= "https://github.com/logseq/logseq"
           (editor/extract-nearest-link-from-text
            "[logseq](https://github.com/logseq/logseq) is #awesome :)" 0 editor/url-regex))
        "Finds url in markdown link correctly"))

  (is (= "https://github.com/logseq/logseq"
         (editor/extract-nearest-link-from-text
          "[[https://github.com/logseq/logseq][logseq]] is #awesome :)" 0 editor/url-regex))
      "Finds url in org link correctly"))

(defn- keyup-handler
  "Spied version of editor/keyup-handler"
  [{:keys [value cursor-pos action commands]
    ;; Default to some commands matching which matches default behavior for most
    ;; completion scenarios
    :or {commands [:fake-command]}}]
  ;; Reset editor action in order to test result
  (state/set-editor-action! action)
  ;; Default cursor pos to end of line
  (let [pos (or cursor-pos (count value))
        input #js {:value value}
        command (subs value 1)]
    (with-redefs [editor/get-last-command (constantly command)
                  editor/get-matched-commands (constantly commands)
                  ;; Ignore as none of its behaviors are tested
                  editor/default-case-for-keyup-handler (constantly nil)
                  cursor/pos (constantly pos)]
      ((editor/keyup-handler nil input)
       #js {:key (subs value (dec (count value)))}
       nil))))

(deftest keyup-handler-test
  (testing "Command autocompletion"
    ;; default last matching command is ""
    (keyup-handler {:value "/z"
                    :action :commands
                    :commands []})
    (is (= :commands (state/get-editor-action))
        "Completion stays open if no matches but differs from last success by <= 2 chars")

    (keyup-handler {:value "/zz"
                    :action :commands
                    :commands []})
    (is (= :commands (state/get-editor-action))
        "Completion stays open if no matches but differs from last success by <= 2 chars")

    (keyup-handler {:value "/zzz"
                    :action :commands
                    :commands []})
    (is (= nil (state/get-editor-action))
        "Completion closed if no matches and > 2 chars form last success")

    (keyup-handler {:value "/b"
                    :action :commands
                    :commands [:fake-command]})
    (is (= :commands (state/get-editor-action))
        "Completion stays open if there is a matching command")

    (keyup-handler {:value "/ " :action :commands})
    (is (= nil (state/get-editor-action))
        "Completion closed after a space follows /")

    (keyup-handler {:value "/block " :action :commands})
    (is (= :commands (state/get-editor-action))
        "Completion stays open if space is part of the search term for /"))

  (testing "Tag autocompletion"
    (keyup-handler {:value "foo #b" :action :page-search-hashtag})
    (is (= :page-search-hashtag (state/get-editor-action))
        "Completion stays open for one tag")

    (keyup-handler {:value "text # #bar"
                    :action :page-search-hashtag
                    :cursor-pos 6})
    (is (= :page-search-hashtag (state/get-editor-action))
        "Completion stays open when typing tag before another tag"))
  ;; Reset state
  (state/set-editor-action! nil))

(defn- handle-last-input-handler
  "Spied version of editor/handle-last-input"
  [{:keys [value cursor-pos]}]
  ;; Reset editor action in order to test result
  (state/set-editor-action! nil)
  ;; Default cursor pos to end of line
  (let [pos (or cursor-pos (count value))]
    (with-redefs [state/get-input (constantly #js {:value value})
                  cursor/pos (constantly pos)
                  cursor/move-cursor-backward (constantly nil) ;; ignore if called
                  cursor/get-caret-pos (constantly {})]
      (editor/handle-last-input))))

(deftest handle-last-input-handler-test
  (testing "Command autocompletion"
    (handle-last-input-handler {:value "/"})
    (is (= :commands (state/get-editor-action))
        "Command search if only / has been typed")

    (handle-last-input-handler {:value "some words /"})
    (is (= :commands (state/get-editor-action))
        "Command search on start of new word")

    (handle-last-input-handler {:value "a line\n/"})
    (is (= :commands (state/get-editor-action))
        "Command search on start of a new line")

    (handle-last-input-handler {:value "https://"})
    (is (= nil (state/get-editor-action))
        "No command search in middle of a word")

    (handle-last-input-handler {:value "#blah/"})
    (is (= nil (state/get-editor-action))
        "No command search after a tag search to allow for namespace completion"))

  (testing "Tag autocompletion"
    (handle-last-input-handler {:value "#"
                                :cursor-pos 1})
    (is (= :page-search-hashtag (state/get-editor-action))
        "Page search if only hashtag has been typed")

    (handle-last-input-handler {:value "foo #"
                                :cursor-pos 5})
    (is (= :page-search-hashtag (state/get-editor-action))
        "Page search if hashtag has been typed at EOL")

    (handle-last-input-handler {:value "#Some words"
                                :cursor-pos 1})
    (is (= :page-search-hashtag (state/get-editor-action))
        "Page search if hashtag is at start of line and there are existing words")

    (handle-last-input-handler {:value "foo #"
                                :cursor-pos 5})
    (is (= :page-search-hashtag (state/get-editor-action))
        "Page search if hashtag is at EOL and after a space")

    (handle-last-input-handler {:value "foo #bar"
                                :cursor-pos 5})
    (is (= :page-search-hashtag (state/get-editor-action))
        "Page search if hashtag is in middle of line and after a space")

    (handle-last-input-handler {:value "String#" :cursor-pos 7})
    (is (= nil (state/get-editor-action))
        "No page search if hashtag has been typed at end of a word")

    (handle-last-input-handler {:value "foo#bar" :cursor-pos 4})
    (is (= nil (state/get-editor-action))
        "No page search if hashtag is in middle of word")

    (handle-last-input-handler {:value "`String#gsub and String#`"
                                :cursor-pos (dec (count "`String#gsub and String#`"))})
    (is (= nil (state/get-editor-action))
        "No page search within backticks"))
  ;; Reset state
  (state/set-editor-action! nil))

(deftest save-block!
  (testing "Saving blocks with and without properties"
    (test-helper/load-test-files [{:page {:block/title "foo"}
                                   :blocks [{:block/title "foo"
                                             :build/properties {:logseq.property/heading 1}}]}])
    (let [repo test-helper/test-db
          page-uuid (:block/uuid (db/get-page "foo"))
          block-uuid (:block/uuid (model/get-block-by-page-name-and-block-route-name repo (str page-uuid) "foo"))]
      (editor/save-block! repo block-uuid "# bar")
      (is (= "bar" (:block/title (model/query-block-by-uuid block-uuid))))

      (editor/save-block! repo block-uuid "# bar")
      (is (= "bar" (:block/title (model/query-block-by-uuid block-uuid)))))))
