;;; treesit-chunking-test.el --- Tests for treesit-chunking -*- lexical-binding: t; -*-

;; Copyright (C) 2025 yqrashawn

;; Author: yqrashawn <namy.19@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive test suite for treesit-chunking functionality.

;;; Code:

(require 'ert)
(require 'treesit-chunking)

(comment
  (ert "treesit-"))
;;; Test Data and Fixtures

(defconst treesit-chunking-test--elisp-sample
  "(defun test-function (arg1 arg2)
  \"Test function docstring.\"
  (let ((result (+ arg1 arg2)))
    (message \"Result: %s\" result)
    result))

(defvar test-variable 42
  \"Test variable.\")

(defmacro test-macro (body)
  \"Test macro.\"
  `(progn ,@body))"
  "Sample Elisp code for testing.")

(defconst treesit-chunking-test--clojure-sample
  "(ns test.core
  (:require [clojure.string :as str]))

(defn test-function
  \"Test function docstring.\"
  [arg1 arg2]
  (let [result (+ arg1 arg2)]
    (println \"Result:\" result)
    result))

(def test-variable 42)

(defprotocol TestProtocol
  (test-method [this]))"
  "Sample Clojure code for testing.")

(defconst treesit-chunking-test--javascript-sample
  "import { helper } from './utils.js';

function testFunction(arg1, arg2) {
  // Test function
  const result = arg1 + arg2;
  console.log('Result:', result);
  return result;
}

class TestClass {
  constructor(value) {
    this.value = value;
  }

  method() {
    return this.value * 2;
  }
}

export { testFunction, TestClass };"
  "Sample JavaScript code for testing.")

(defconst treesit-chunking-test--python-sample
  "import os
from typing import List

def test_function(arg1: int, arg2: int) -> int:
    \"\"\"Test function docstring.\"\"\"
    result = arg1 + arg2
    print(f'Result: {result}')
    return result

class TestClass:
    \"\"\"Test class.\"\"\"

    def __init__(self, value):
        self.value = value

    def method(self):
        return self.value * 2"
  "Sample Python code for testing.")

;;; Core Functionality Tests

(ert-deftest treesit-chunking-test-language-detection ()
  "Test language detection from file extensions and major modes."
  (should (eq (treesit-chunking--normalize-language 'js) 'javascript))
  (should (eq (treesit-chunking--normalize-language 'clj) 'clojure))
  (should (eq (treesit-chunking--normalize-language 'py) 'python))
  (should (eq (treesit-chunking--normalize-language 'el) 'elisp))
  (should (eq (treesit-chunking--normalize-language 'typescript) 'typescript)))

(ert-deftest treesit-chunking-test-language-support ()
  "Test language support detection."
  ;; Note: These tests may fail if tree-sitter grammars are not installed
  ;; We'll test the logic without requiring actual tree-sitter availability
  (let ((supported-langs '(javascript typescript python clojure)))
    (dolist (lang supported-langs)
      (should (assoc lang treesit-chunking-splittable-node-types)))))

(ert-deftest treesit-chunking-test-splittable-node-types ()
  "Test that splittable node types are defined for supported languages."
  (should (treesit-chunking--get-splittable-types 'javascript))
  (should (treesit-chunking--get-splittable-types 'python))
  (should (treesit-chunking--get-splittable-types 'clojure))
  (should (member 'function_declaration (treesit-chunking--get-splittable-types 'javascript)))
  (should (member 'class_declaration (treesit-chunking--get-splittable-types 'javascript))))

;;; Symbol Extraction Tests

(ert-deftest treesit-chunking-test-extract-symbols-defined-elisp ()
  "Test symbol extraction for Elisp definitions."
  (let ((symbols (treesit-chunking--extract-symbols-defined
                  treesit-chunking-test--elisp-sample 'elisp)))
    (should (cl-find-if (lambda (s) (string= (plist-get s :name) "test-function")) symbols))
    (should (cl-find-if (lambda (s) (string= (plist-get s :name) "test-variable")) symbols))
    (should (cl-find-if (lambda (s) (string= (plist-get s :name) "test-macro")) symbols))
    (should (cl-find-if (lambda (s) (eq (plist-get s :type) 'function)) symbols))
    (should (cl-find-if (lambda (s) (eq (plist-get s :type) 'variable)) symbols))
    (should (cl-find-if (lambda (s) (eq (plist-get s :type) 'macro)) symbols))))

(ert-deftest treesit-chunking-test-extract-symbols-defined-clojure ()
  "Test symbol extraction for Clojure definitions."
  (let ((symbols (treesit-chunking--extract-symbols-defined
                  treesit-chunking-test--clojure-sample 'clojure)))
    (should (cl-find-if (lambda (s) (string= (plist-get s :name) "test-function")) symbols))
    (should (cl-find-if (lambda (s) (string= (plist-get s :name) "test-variable")) symbols))
    (should (cl-find-if (lambda (s) (string= (plist-get s :name) "TestProtocol")) symbols))
    (should (cl-find-if (lambda (s) (eq (plist-get s :type) 'function)) symbols))
    (should (cl-find-if (lambda (s) (eq (plist-get s :type) 'variable)) symbols))
    (should (cl-find-if (lambda (s) (eq (plist-get s :type) 'protocol)) symbols))))

(ert-deftest treesit-chunking-test-extract-symbols-used ()
  "Test extraction of used symbols."
  (let ((used-symbols (treesit-chunking--extract-enhanced-symbols-used
                       treesit-chunking-test--elisp-sample 'elisp)))
    (should (member "message" used-symbols))
    (should (member "progn" used-symbols))
    ;; Should not include defined symbols
    (should-not (member "test-function" used-symbols))
    ;; Should not include filtered keywords
    (should-not (member "let" used-symbols))))

;;; Dependency Extraction Tests

(ert-deftest treesit-chunking-test-extract-dependencies-clojure ()
  "Test dependency extraction for Clojure."
  (let ((deps (treesit-chunking--extract-enhanced-dependencies
               treesit-chunking-test--clojure-sample 'clojure)))
    (should (cl-find-if (lambda (d) (string= (plist-get d :name) "test.core")) deps))
    (should (cl-find-if (lambda (d) (eq (plist-get d :type) 'namespace)) deps))))

(ert-deftest treesit-chunking-test-extract-dependencies-javascript ()
  "Test dependency extraction for JavaScript."
  (let ((deps (treesit-chunking--extract-enhanced-dependencies
               treesit-chunking-test--javascript-sample 'javascript)))
    (should (cl-find-if (lambda (d) (string= (plist-get d :name) "./utils.js")) deps))
    (should (cl-find-if (lambda (d) (eq (plist-get d :type) 'import-from)) deps))))

(ert-deftest treesit-chunking-test-extract-dependencies-python ()
  "Test dependency extraction for Python."
  (let ((deps (treesit-chunking--extract-enhanced-dependencies
               treesit-chunking-test--python-sample 'python)))
    (should (cl-find-if (lambda (d) (string= (plist-get d :name) "os")) deps))
    (should (cl-find-if (lambda (d) (string= (plist-get d :name) "typing")) deps))
    (should (cl-find-if (lambda (d) (eq (plist-get d :type) 'import)) deps))
    (should (cl-find-if (lambda (d) (eq (plist-get d :type) 'from-import)) deps))))

;;; Chunk Structure Tests

(ert-deftest treesit-chunking-test-chunk-creation ()
  "Test basic chunk structure creation."
  (let ((chunk (make-treesit-chunking-chunk
                :content "test content"
                :start-line 1
                :end-line 5
                :language 'elisp
                :file-path "/test/file.el"
                :node-type "function_declaration")))
    (should (string= (treesit-chunking-chunk-content chunk) "test content"))
    (should (= (treesit-chunking-chunk-start-line chunk) 1))
    (should (= (treesit-chunking-chunk-end-line chunk) 5))
    (should (eq (treesit-chunking-chunk-language chunk) 'elisp))
    (should (string= (treesit-chunking-chunk-file-path chunk) "/test/file.el"))
    (should (string= (treesit-chunking-chunk-node-type chunk) "function_declaration"))))

(ert-deftest treesit-chunking-test-chunk-to-plist ()
  "Test chunk to plist conversion."
  (let* ((chunk (make-treesit-chunking-chunk
                 :content "test content"
                 :start-line 1
                 :end-line 5
                 :language 'elisp
                 :file-path "/test/file.el"
                 :node-type "function_declaration"
                 :definition-name "test-func"
                 :complexity-score 5))
         (plist (treesit-chunking-chunk-to-plist chunk)))
    (should (string= (plist-get plist :content) "test content"))
    (should (= (plist-get plist :start-line) 1))
    (should (eq (plist-get plist :language) 'elisp))
    (should (string= (plist-get plist :definition-name) "test-func"))
    (should (= (plist-get plist :complexity-score) 5))))

;;; Fallback Strategy Tests

(ert-deftest treesit-chunking-test-simple-line-split ()
  "Test simple line-based splitting fallback."
  (let* ((content (concat "line 1\nline 2\nline 3\nline 4\nline 5"))
         (treesit-chunking-chunk-size 20) ; Small size to force splitting
         (chunks (treesit-chunking--simple-line-split content 'text "/test/file.txt")))
    (should (> (length chunks) 1))
    (should (cl-every (lambda (chunk)
                        (<= (length (treesit-chunking-chunk-content chunk))
                            treesit-chunking-chunk-size))
                      chunks))
    (should (cl-every (lambda (chunk)
                        (string= (treesit-chunking-chunk-node-type chunk) "simple-line"))
                      chunks))))

(ert-deftest treesit-chunking-test-intelligent-line-split ()
  "Test intelligent line boundary detection."
  (let* ((content "function test() {\n  return true;\n}\n\nclass Test {\n  method() {}\n}")
         (chunks (treesit-chunking--intelligent-line-split content 'javascript "/test/file.js")))
    (should (> (length chunks) 0))
    (should (cl-some (lambda (chunk)
                       (string-match-p "function test" (treesit-chunking-chunk-content chunk)))
                     chunks))))

;;; Function Name Extraction Tests

(ert-deftest treesit-chunking-test-extract-function-name ()
  "Test function name extraction from different node types."
  (should (string= (treesit-chunking--extract-function-name
                    "function testFunc() { return true; }"
                    "function_declaration")
                   "testFunc"))
  (should (string= (treesit-chunking--extract-function-name
                    "const arrow = () => {}"
                    "arrow_function")
                   "arrow"))
  (should (string= (treesit-chunking--extract-function-name
                    "(defn clojure-func [])"
                    "defn")
                   "clojure-func"))
  (should (string= (treesit-chunking--extract-function-name
                    "class TestClass {}"
                    "class_declaration")
                   "TestClass")))

;;; Complexity Calculation Tests

(ert-deftest treesit-chunking-test-complexity-calculation ()
  "Test complexity score calculation."
  (let ((simple-content "const x = 1;")
        (complex-content "function complex() {
          if (condition) {
            for (let i = 0; i < 10; i++) {
              if (another && condition || third) {
                while (loop) {
                  // complex logic
                }
              }
            }
          }
        }"))
    (should (> (treesit-chunking--calculate-complexity complex-content)
               (treesit-chunking--calculate-complexity simple-content)))))

;;; String Splitting and Chunk Management Tests

(ert-deftest treesit-chunking-test-split-string-basic ()
  "Test basic string splitting functionality."
  ;; This test assumes tree-sitter is not available, so it should fall back
  (let* ((treesit-chunking-fallback-strategies '(simple-line))
         (chunks (treesit-chunking-split-string
                  "line1\nline2\nline3\nline4\nline5\nline6"
                  'unsupported-lang)))
    (should (> (length chunks) 0))
    (should (cl-every (lambda (chunk)
                        (treesit-chunking-chunk-p chunk))
                      chunks))))

(ert-deftest treesit-chunking-test-add-overlap ()
  "Test chunk overlap addition."
  (let* ((chunk1 (make-treesit-chunking-chunk :content "content1"))
         (chunk2 (make-treesit-chunking-chunk :content "content2"))
         (chunk3 (make-treesit-chunking-chunk :content "content3"))
         (chunks (list chunk1 chunk2 chunk3))
         (treesit-chunking-chunk-overlap 5)
         (overlapped (treesit-chunking--add-overlap chunks)))
    (should (= (length overlapped) 3))
    ;; Second chunk should have content from first chunk (if overlap works)
    (when (> (length overlapped) 1)
      (should (string-match-p "tent1"
                              (treesit-chunking-chunk-content (nth 1 overlapped)))))))

;;; Configuration Tests

(ert-deftest treesit-chunking-test-configuration ()
  "Test configuration variables."
  (should (numberp treesit-chunking-chunk-size))
  (should (> treesit-chunking-chunk-size 0))
  (should (numberp treesit-chunking-chunk-overlap))
  (should (>= treesit-chunking-chunk-overlap 0))
  (should (numberp treesit-chunking-min-chunk-size))
  (should (> treesit-chunking-min-chunk-size 0))
  (should (booleanp treesit-chunking-extract-symbols))
  (should (booleanp treesit-chunking-track-dependencies)))

;;; Integration Tests

(ert-deftest treesit-chunking-test-end-to-end-elisp ()
  "Test end-to-end processing of Elisp code."
  (with-temp-buffer
    (insert treesit-chunking-test--elisp-sample)
    (emacs-lisp-mode)
    (let* ((chunks (treesit-chunking-split-buffer 'elisp "test.el")))
      (should (> (length chunks) 0))
      (should (cl-every (lambda (chunk)
                          (and (treesit-chunking-chunk-p chunk)
                               (eq (treesit-chunking-chunk-language chunk) 'elisp)))
                        chunks)))))

;;; Error Handling Tests

(ert-deftest treesit-chunking-test-empty-content ()
  "Test handling of empty content."
  (let ((chunks (treesit-chunking-split-string "" 'elisp)))
    (should (or (null chunks)
                (and (= (length chunks) 1)
                     (string-empty-p (string-trim (treesit-chunking-chunk-content (car chunks)))))))))

(ert-deftest treesit-chunking-test-unsupported-language ()
  "Test handling of unsupported languages."
  (let ((chunks (treesit-chunking-split-string "some content" 'unsupported)))
    ;; Should fall back to simple splitting
    (should (> (length chunks) 0))))

;;; Performance Tests

(ert-deftest treesit-chunking-test-large-content ()
  "Test handling of large content."
  (let* ((large-content (make-string 10000 ?a))
         (chunks (treesit-chunking-split-string large-content 'text)))
    (should (> (length chunks) 0))
    ;; Should split large content into multiple chunks
    (should (or (> (length chunks) 1)
                (<= (length (treesit-chunking-chunk-content (car chunks)))
                    treesit-chunking-chunk-size)))))

;;; Utility Function Tests

(ert-deftest treesit-chunking-test-normalize-definition-type ()
  "Test definition type normalization."
  (should (string= (treesit-chunking--normalize-definition-type "function_declaration") "function"))
  (should (string= (treesit-chunking--normalize-definition-type "class_declaration") "class"))
  (should (string= (treesit-chunking--normalize-definition-type "interface_declaration") "interface"))
  (should (string= (treesit-chunking--normalize-definition-type "unknown_type") "unknown_type")))

(ert-deftest treesit-chunking-test-docstring-extraction ()
  "Test docstring extraction from different languages."
  (should (string= (treesit-chunking--extract-docstring
                    "(defn test \"This is a docstring\" [])" 'clojure)
                   "This is a docstring"))
  (should (string= (treesit-chunking--extract-docstring
                    "def test():\n    \"\"\"This is a docstring\"\"\"" 'python)
                   "This is a docstring"))
  (should (string-match-p "Test function"
                          (or (treesit-chunking--extract-docstring
                               "/**\n * Test function\n */" 'javascript) ""))))

(provide 'treesit-chunking-test)
;;; treesit-chunking-test.el ends here
