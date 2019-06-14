;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: foo
;;;;    System: CogSketch v4, w local UI
;;;;    Author: 
;;;;   Created: June 13, 2019 21:06:54
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedBy: Kezhen $
;;;; ---------------------------------------------------------------------------

(in-package :cogsketch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-target-pipeline ()
  (let* ((case-lib-name 'NeuronCaseLibrary)
         (target-case-name 'TargetNeuronMt)
         (probe-name 'NeuronProbeMt)
         (subsketches (get-subsketches-from-current-sketch))
         (subsketch (first (last subsketches))) ; grab the first one ordered in the GUI
         (raw-facts (get-facts-from-subsketch subsketch))
         (sme-facts (apply-filter raw-facts))
         (store-as-case-ret (store-as-case sme-facts target-case-name))
         (retrieved-cases (fire:ask-it `(d::reminding (d::KBCaseFn ,probe-name)
                                                      (d::CaseLibraryFn ,case-lib-name)
                                                      (d::TheSet)
                                                      ?case
                                                      ?sme)
                            :response '(?case ?sme)))
         (case-for-matching (caar retrieved-cases))
         (first-mapping (first (fire::ask-it `d::(and (matchBetween
                                                       (KBCaseFn ,case-for-matching)
                                                       (KBCaseFn ,target-case-name)
                                                       (TheSet) ?match)
                                                      (bestMapping ?match ?mapping))
                                 :response '?mapping))))
    (fire::ask-it `(d::and (d::candidateInferenceOf ?ci ,first-mapping)
                        (d::candidateInferenceContent ?ci ?content))
      :response '?content)))

(defun get-subsketches-from-current-sketch ()
  (let ((current-sketch (cog::sketches cog::*cogsketch-core*)))
     (cog::subsketches (first current-sketch))))

(defun get-facts-from-subsketch (subsketch)
  """ subsketch is a sketch object, not a listp "
  ; facts about the sketch are only accessible via that Layer-level, so we do the following to get there:
  (let ((subsketch-obj-name (cog::object-name subsketch)))
    ; find the facts we care about as triples of glyph, object, collection facts and put it in Working Memory (WM)
    (fire::ask-it '(and (glyphRepresentsObject ?glyph ?object) (isa ?object ?coll))
      :context subsketch-obj-name
      :response '(?glyph ?object ?coll))

    ; retrieve those facts from WM (this is over 1000 facts, so there's still some work we have to do)
    (fire::wm-retrieve-it (list 'ist-Information subsketch-obj-name '?fact) :response '?fact)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
