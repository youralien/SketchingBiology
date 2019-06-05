;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: case-analogy-pipeline
;;;;    System: CogSketch v4, w local UI
;;;;    Author: 
;;;;   Created: May 27, 2019 09:02:26
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedBy: Ryan $
;;;; ---------------------------------------------------------------------------

;;; (in-package :cogsketch)
(in-package :cl-user)
;(in-package :cg-user)
; NOTE: load cogsketch
; NOTE: load fire

(setq case-lib-name 'NeuronCaseLibrary)

(defun run-target-pipeline ()
  
  (setq subsketches (get-subsketches-from-current-sketch))
  (setq subsketch (first (last subsketches))) ; grab the first one ordered in the GUI
  (setq raw-facts (get-facts-from-subsketch subsketch))
  (setq sme-facts (apply-filter raw-facts))
  
  ;;; target creation
  (setq target-case-name 'TargetNeuronMt)
  (store-as-case sme-facts target-case-name)
  ;;; (add-case-to-library target-case-name 'NeuronCaseLibrary)
  
  
  ;;; probe creation
  (setq probe-name 'NeuronProbeMt)
  
  ;;; todo: use probe instead of case-name
  (setq retrieved-cases
    (fire:ask-it `(d::reminding (d::KBCaseFn ,case-name)
                                (d::CaseLibraryFn ,case-lib-name)
                                (d::TheSet)
                                ?case
                                ?sme)
      :response '(?case ?sme)))
  
  (format t "retrieved-cases: ~A" retrieved-cases)
  ;; todo (caar retrieved-cases) 
  (setq case-for-matching (caar retrieved-cases))
  
  (format t "case-for-matching: ~A" case-for-matching)
  
  
  (setq mappings (fire::ask-it `d::(and (matchBetween
                                     (KBCaseFn ,case-for-matching)
                                     (KBCaseFn ,target-case-name)
                           (TheSet) ?match)
                           (bestMapping ?match ?mapping))
                   :response '?mapping))
  
  (format t "mappings: ~A" mappings)
  
  (setq mapping1 (car mappings))
  
  (setq cis (fire::ask-it `(d::and (d::candidateInferenceOf ?ci ,mapping1)
                                (d::candidateInferenceContent ?ci ?content))
              :response '?content))
  
  (format t "cis: ~A" cis)
  cis
)

(defun run-train-pipeline ()
  """ building the case from N subsketches in an opened sk file """
  (setq subsketches (get-subsketches-from-current-sketch))
  (setq iterator 0)
  ; iterate from the back
  (dolist (subsketch subsketches)
    (setq raw-facts (get-facts-from-subsketch subsketch))
    (setq sme-facts (apply-filter raw-facts))
    (setq case-name (intern (format nil "~a~a" "CaseFacts" iterator)))
    (store-as-case sme-facts case-name)
    (add-case-to-library case-name case-lib-name)
    (incf iterator)
    (pprint case-name)
    )
  

  ; TODO: get conceptual relation oracle labels from the facts too
  ; TODO: use SME to find best matches
  ; TODO: transfer conceptual relations to target sketch
  )

(defun get-subsketches-from-current-sketch()
  (setq current-sketch (cog::sketches cog::*cogsketch-core*))
  (setq subsketches (cog::subsketches (first current-sketch)))
  subsketches
  )

(defun get-first-subsketch(subsketches)
  """ TODO: construct cases from sk file """
  ; grabs the current sketch from CogSketch
  ; we choose to use the first subsketch (of the 10 for neuron, for example)
  (setq subsketch (last subsketches))
  subsketch
  )


(defun get-facts-from-subsketch (subsketch)
  """ subsketch is a sketch object, not a listp "
  ; facts about the sketch are only accessible via that Layer-level, so we do the following to get there:
  (setq subsketch-obj-name (cog::object-name subsketch))
  ; (setq layer-voronoi (cog::layers (first subsketch)))
  ; (setq layer1 (first layer-voronoi))

  ; find the facts we care about as triples of glyph, object, collection facts and put it in Working Memory (WM)
  (fire::ask-it '(and (glyphRepresentsObject ?glyph ?object) (isa ?object ?coll))
                :context subsketch-obj-name
                :response '(?glyph ?object ?coll))

  ; retrieve those facts from WM (this is over 1000 facts, so there's still some work we have to do)
  ; (fire::wm-retrieve-it '(ist-Information SubsketchContext-OohOompiwhGaxEghowp ?fact) :response '?fact)
  (setq facts (fire::wm-retrieve-it (list 'ist-Information subsketch-obj-name '?fact) :response '?fact))
  facts
  )

(defun otherRCC8Facts (relation)
  (or (eql relation 'rcc8-DC)
      (eql relation 'rcc8-EC)
      (eql relation 'rcc8-PO)
      (eql relation 'rcc8-EQ)
      (eql relation 'rcc8-TPP)
      (eql relation 'rcc8-TPPi)
      (eql relation 'rcc8-NTPP)
      (eql relation 'rcc8-NTPPi)))

(defun conceptualFacts-p (relation)
  (or (eql relation 'inRegion)
      (eql relation 'connectedAtEnd)
      (eql relation 'connectedAlongSurface)
      (eql relation 'surfaceParts)
      (eql relation 'spiralsAround)
      (eql relation 'mainFunctionalComponent)
      ;; (eql relation ')
      ))

(defun SMEFacts-p (fact)
  (or (eql (car fact) 'hasRCC8Relation)
      (eql (car fact) 'isa)
      (otherRCC8Facts (car fact))
      (conceptualFacts-p (car fact))))

(defun apply-filter (facts)
  (do ((lsts facts (cdr lsts))
     (fact nil (car lsts))
     (filteredlst nil (cond ((SMEFacts-p fact) 
                             (cons fact filteredlst))
                            (t filteredlst))))
      ((null lsts) filteredlst)))

(defun store-as-case (sme-facts case-name)
  (dolist (f sme-facts)
    (fire::kb-store f :mt case-name)))

(defun add-case-to-library (case-name case-library-name)
  (fire::add-to-caseLib case-name case-library-name))
#|
(defun load-facts-from-text (path)
  (setq facts nil)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
         ;;; (facts nil (cons (read-from-string line) facts)))
        ((eql line 'eof) facts)
      (format t "~A~%" (collect line)))))
;;;      (setq facts (append facts (list (read-from-string line))))))
;;;  facts)
|#

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
        while line
        collect line)))

(defun write-list(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (segment l)
      (format out "~A" segment)
      (format out "~%"))))

;;; (setq facts (get-file "C:\\Users\\qrgtablets\\Desktop\\TargetCaseFactsNeuron1.krf"))
;;;(setq facts (load-facts-from-text "C:\\Users\\qrgtablets\\Desktop\\TargetCaseFactsNeuron1.krf"))
;;;(setq filtered (mapcar #'(lambda (x) (read-from-string x)) facts))
