;; ===========================================================
;;  The Parser is the link between the user input and the
;;  Interpreter. The Parser reads a string from the user,
;;  'understands' it't meaning and creates the PrologFunctors
;;  that it describes.
;;
(ns logic.parser
  [:use [logic.term]])
