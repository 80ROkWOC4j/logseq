(ns frontend.handler.editor-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [frontend.commands :as commands]
            [frontend.db :as db]
            [frontend.db.model :as model]
            [frontend.handler.editor :as editor]
            [frontend.state :as state]
            [frontend.test.helper :as test-helper]
            [frontend.util :as util]
            [frontend.util.cursor :as cursor]))

(use-fixtures :each test-helper/start-and-destroy-db)

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

(deftest normalize-keydown-key-test
  (testing "IME Process key is normalized for autopair chars"
    (is (= "("
           (#'editor/normalize-keydown-key
            #js {:key "Process"
                 :shiftKey true
                 :event_ #js {:code "Digit9"}})))

    (is (= "`"
           (#'editor/normalize-keydown-key
            #js {:key "Process"
                 :shiftKey false
                 :event_ #js {:code "Backquote"}})))

    (is (= "["
           (#'editor/normalize-keydown-key
            #js {:key "Process"
                 :shiftKey false
                 :event_ #js {:code "BracketLeft"}}))))

  (testing "IME key can be normalized via native goog browser event"
    (is (= "("
           (#'editor/normalize-keydown-key
            #js {:key "Unidentified"
                 :keyCode 229
                 :getBrowserEvent (fn []
                                    #js {:code "Digit9"
                                         :shiftKey true
                                         :isComposing false})}))))

  (testing "Unknown Process key remains unchanged"
    (is (= "Process"
           (#'editor/normalize-keydown-key
            #js {:key "Process"
                 :shiftKey false
                 :event_ #js {:code "KeyA"}}))))

  (testing "Non-Process key is unchanged"
    (is (= "("
           (#'editor/normalize-keydown-key
            #js {:key "("
                 :shiftKey true
                 :event_ #js {:code "Digit9"}})))))

(deftest editor-ime-active-test
  (testing "Uses editor composition state"
    (with-redefs [state/editor-in-composition? (constantly true)]
      (is (true? (#'editor/editor-ime-active? #js {})))))

  (testing "Detects composition events and process keys"
    (with-redefs [state/editor-in-composition? (constantly false)]
      (is (true? (#'editor/editor-ime-active? #js {:type "compositionstart"})))
      (is (true? (#'editor/editor-ime-active? #js {:key "Process"})))
      (is (true? (#'editor/editor-ime-active? #js {:keyCode 229})))
      (is (false? (#'editor/editor-ime-active? #js {:type "keyup"
                                                    :key "a"
                                                    :keyCode 65}))))))

(deftest ime-post-autopair-close-char-test
  (with-redefs [state/get-editor-action (constantly nil)
                util/get-selected-text (constantly "")]
    (testing "Adds closing char for IME processed backtick"
      (is (= "`"
             (#'editor/ime-post-autopair-close-char
              #js {:value "`"}
              1
              true))))

    (testing "Adds closing paren only in allowed contexts"
      (is (= ")"
             (#'editor/ime-post-autopair-close-char
              #js {:value " ("}
              2
              true)))
      (is (nil?
           (#'editor/ime-post-autopair-close-char
            #js {:value "a("}
            2
            true))))

    (testing "Skips when already closed or not IME processed"
      (is (nil?
           (#'editor/ime-post-autopair-close-char
            #js {:value "()"}
            1
            true)))
      (is (nil?
           (#'editor/ime-post-autopair-close-char
            #js {:value "`"}
            1
            false)))))

    (testing "Uses inserted char hint from input event data"
      (is (= "`"
             (#'editor/ime-post-autopair-close-char
              #js {:value "`"}
              1
              true
              "`"))))

    (testing "Backtick can keep generating pairs for IME sequence"
      (is (= "`"
             (#'editor/ime-post-autopair-close-char
              #js {:value "```"}
              2
              true
              "`"))))

    (testing "Backtick can keep generating pairs even without input data hint"
      (is (= "`"
             (#'editor/ime-post-autopair-close-char
              #js {:value "```"}
              2
              true))))

    (testing "Backtick can keep generating pairs at first pair boundary"
      (is (= "`"
             (#'editor/ime-post-autopair-close-char
              #js {:value "```"}
              1
              true
              "`"))))

    (testing "Backtick first pair boundary also works without input data hint"
      (is (= "`"
             (#'editor/ime-post-autopair-close-char
              #js {:value "```"}
              1
              true))))

  (with-redefs [state/get-editor-action (constantly :page-search)
                util/get-selected-text (constantly "")]
    (testing "Skips when editor action is active"
      (is (nil?
           (#'editor/ime-post-autopair-close-char
            #js {:value "["}
            1
            true))))))

(deftest event-input-data-char-test
  (is (= "`" (#'editor/event-input-data-char #js {:nativeEvent #js {:data "`"}})))
  (is (= "(" (#'editor/event-input-data-char #js {:data "("})))
  (is (nil? (#'editor/event-input-data-char #js {:nativeEvent #js {:data "ab"}})))
  (is (nil? (#'editor/event-input-data-char #js {}))))

(deftest recover-ime-input-char-test
  (testing "Recover missing IME punctuation char"
    (let [calls (atom [])]
      (with-redefs [cursor/pos (constantly 0)
                    commands/simple-insert! (fn [id value opts]
                                              (swap! calls conj [id value opts]))]
        (is (true? (#'editor/recover-ime-input-char! "id" #js {:value ""} "`")))
        (is (= [["id" "`" nil]] @calls)))))

  (testing "Do not recover when char already exists"
    (let [calls (atom [])]
      (with-redefs [cursor/pos (constantly 1)
                    commands/simple-insert! (fn [id value opts]
                                              (swap! calls conj [id value opts]))]
        (is (nil? (#'editor/recover-ime-input-char! "id" #js {:value "`"} "`")))
        (is (empty? @calls)))))

  (testing "Do not recover for non-autopair chars"
    (let [calls (atom [])]
      (with-redefs [cursor/pos (constantly 0)
                    commands/simple-insert! (fn [id value opts]
                                              (swap! calls conj [id value opts]))]
        (is (nil? (#'editor/recover-ime-input-char! "id" #js {:value ""} "a")))
        (is (empty? @calls))))))

(deftest ime-handle-backtick-existing-close-test
  (testing "Consumes IME inserted backtick when closing existing pair"
    (let [set-calls (atom [])
          move-calls (atom [])]
      (with-redefs [cursor/pos (constantly 3)
                    state/set-block-content-and-last-pos! (fn [id value pos]
                                                            (swap! set-calls conj [id value pos]))
                    cursor/move-cursor-to (fn [_input pos]
                                            (swap! move-calls conj pos))]
        (is (true? (#'editor/ime-handle-backtick-existing-close! "id" #js {:value "`a``"})))
        (is (= [["id" "`a`" 3]] @set-calls))
        (is (= [3] @move-calls)))))

  (testing "Does not consume when typing repeated backticks"
    (let [set-calls (atom [])]
      (with-redefs [cursor/pos (constantly 2)
                    state/set-block-content-and-last-pos! (fn [id value pos]
                                                            (swap! set-calls conj [id value pos]))
                    cursor/move-cursor-to (fn [_input _pos] nil)]
        (is (nil? (#'editor/ime-handle-backtick-existing-close! "id" #js {:value "```"})))
        (is (empty? @set-calls))))))

  (testing "Does not consume at first pair boundary"
    (let [set-calls (atom [])]
      (with-redefs [cursor/pos (constantly 1)
                    state/set-block-content-and-last-pos! (fn [id value pos]
                                                            (swap! set-calls conj [id value pos]))
                    cursor/move-cursor-to (fn [_input _pos] nil)]
        (is (nil? (#'editor/ime-handle-backtick-existing-close! "id" #js {:value "``"})))
        (is (empty? @set-calls))))))

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
