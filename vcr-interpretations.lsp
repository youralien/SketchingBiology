;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: vcr-interpretation.lsp
;;;;    System: CogSketch
;;;;    Author: Ken Forbus
;;;;   Created: June 29, 2004 10:20:57
;;;;   Purpose: Visual/Conceptual relationship interpretation
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2019-05-01 16:11:41 -0500 (Wed, 01 May 2019) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :cogsketch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; We use the rules in SKEA-VisualConceptualMappingsMt to find candidate
;;; interpretations for each pair of objects.  These are displayed as 
;;; alternatives on an HTML page.  The user can choose to answer them or not.  
;;; If the user provided an answer already, that answer is displayed, along with 
;;; buttons for allowing them to inspect the rationale for it and retract it if 
;;; desired.
;;;
;;; If they provide an answer, the answer is justified on the basis of both a 
;;; visualInterpretationRelationSuggestion assertion and an assumption using
;;; userAcceptsBinaryRelationSuggestion, which is a first step toward a more 
;;; general representation vocabulary that will be used in our dialogue manager.  
;;; These assertions will be stored with the sketch in a way that there is 
;;; explicit relational structure that can be used in candidate inference 
;;;  evaluation.
;;; 
;;; Note on the microtheory -- CogSketch v3 and v4 use the same KB flat-files,
;;; so we can't change those in ways that would break v3 until v3 is completely
;;; retired.  Once that happens, then we can move the VCR rules to a CogSketch
;;; microtheory instead os a sKEA microtheory.
;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving visual/conceptual possibilities and suggestions

;;; this call returns a list of lists containing:

;; (<entity pair>
;;   <list of suggested relations to ask about>
;;   <list of suggested relations that happen to be true already>
;;   <list of (analogical suggestions . <user acceptance value>)>
;;   <user accepted non-analogical suggested relation>)
;;
;; the three lists of suggested relations and the single user
;; accepted non-analogical relation are mutually exclusive
;;
;; the interface restircts the user to accepting only one non-
;; analogical suggestion at a time, so that is guarenteed to be
;; one or nil
;;
(defun gather-visual-conceptual-relation-questions (subsketch-case reasoner)
  (fire:with-reasoner reasoner
    (let ((suggestions
           (compute-vcr-baseline-and-suggestions subsketch-case 
                                                 :reasoner reasoner))
          ; (analogy-suggestions
          ;  (gather-analogical-vcr-suggestions subsketch-case reasoner))


          ; FIXME!!! damage duty
          (analogy-suggestions
            (run-target-pipeline))

          (accepted
           (fire:ask-it `(d::userAcceptsBinaryRelationSuggestion ?reln ?o1 ?o2)
             :context subsketch-case
             :env nil 
             :infer t 
             :facts :wm
             :response '(?reln ?o1 ?o2)
             :reasoner reasoner)))
      (pprint analogy-suggestions)
      (sort-vcr-suggestions subsketch-case suggestions 
                            analogy-suggestions accepted))))

;;;Example, with simple wheel/axle diagram:
;;;(((Object-3 Object-2) ;; Entities
;;;  ((inRegion Object-3 Object-2) ;; Possible relations
;;;   (objectFoundInLocation Object-3 Object-2)
;;;   (sticksInto Object-3 Object-2)
;;;   (alignedCylinderWithin Object-3 Object-2))
;;;  nil ;; No relns already known
;;;  nil)) ;; No user answer

;;;;; The code below 

(defun compute-vcr-baseline-and-suggestions (case 
                                                &key (reasoner fire::*reasoner*))
  (let ((choices nil))
    (dolist (context (find-vcr-contexts case :reasoner reasoner) choices)
      (setq choices 
        (nconc choices 
               (install-vcr-baseline-and-suggestions
                case (generate-vcr-context-suggestions case context 
                                                       :reasoner reasoner)))))))

(defun find-vcr-contexts (case &key (reasoner fire::*reasoner*))
  ;; Returns entries of the form ((o1 . g1) (o2 . g2) rcc8-fact baseline-relations)
  (fire:with-reasoner reasoner
   (let* ((pose
           (third (third (car 
                          (ltre:fetch-trues
                           (fire:make-case-fact case
                             '(d::subSketchHasPose . ?x)))))))
          (genre 
           (third (third 
                   (car (ltre:fetch-trues 
                         (fire:make-case-fact case 
                           '(d::subSketchHasGenre . ?x)))))))
          (glyph-object-pairs
           (mapcar #'(lambda (fact)
                       (let ((gro (third fact)))
                         (cons (third gro) (second gro))))
             (ltre:fetch-trues (fire:make-case-fact case 
                                 `(d::glyphRepresentsObject
                                   (d::GlyphFn ?o ?layer) ?o)))))
          ;; Pattern above is more specific, since visual/conceptual relations
          ;; don't make sense for annotation glyphs, because they represent
          ;; conceputal entities!
          (results nil))
     (dolist (pair1 glyph-object-pairs results)
       (dolist (pair2 glyph-object-pairs)
         (unless (eq pair1 pair2)
           (let* ((g1 (cdr pair1))
                  (g2 (cdr pair2))
                  (rcc8-fact
                   (third (car (ltre::fetch-trues
                                (fire::make-case-fact
                                    case`(d::hasRCC8Relation ,g1 ,g2 ?reln)))))))
             (when rcc8-fact
               (let ((suggestion-candidates
                      (mapcar #'fire:decontextualize-statement
                        (fire:retrieve-it
                            `(d::visualToConceptualSuggestedRelation 
                              ?g ?p ,(fourth rcc8-fact) ?reln)
                          :context (reasoning-mt (sketch reasoner))
                          :env t)))
                     (baseline-relations nil))
                 (dolist (suggestion-candidate suggestion-candidates)
                   (let ((suggestion-genre (second suggestion-candidate))
                         (suggestion-pose (third suggestion-candidate))
                         (suggested-relation (fifth suggestion-candidate)))
                     (when (and (or (equal genre suggestion-genre)
                                    (fire::spec-of? genre suggestion-genre))
                                (or (equal pose suggestion-pose)
                                    (fire::spec-of? pose suggestion-pose)))
                       (push suggested-relation baseline-relations))))
                 (when baseline-relations
                   (push (list pair1 pair2 rcc8-fact baseline-relations) results)))))))))))

(defun generate-vcr-context-suggestions (case vcr-context 
                                          &key (reasoner fire::*reasoner*))
    ;; vcr-context = ((o1 . g1)(o2 . g2) rcc8-fact baseline-relations)
  ;; [Suspect that baseline-relations will always be a singleton, but we leave open the
  ;;   possibility of more than one.]
  ;; Return: (<context> . alist of (<suggested relation> . <isa justifications>) entries)
  ;; We need to return the isa justifications because someone might change the collections
  ;; associated with one of the entities.  If they do, the arg-isas may no longer be correct.
  ;; We assume that the arg-isa and specpred relations remain constant over the reasoning session,
  ;; so that these properties are not used in any justifications.  Someone might move a glyph,
  ;; so the rcc8 relation in the vcr-context must also be an antecedent of any result.
  (fire::with-reasoner reasoner
    (let* ((sketch (sketch reasoner))
           (reasoning-mt (reasoning-mt sketch))
           (suggestions nil)
           (o1 (caar vcr-context))(o2 (caadr vcr-context))
           (baseline-relations (fourth vcr-context))
           (o1-isas (mapcar #'(lambda (fact)
                                (third (third fact)))
                      (ltre::fetch-trues (fire::make-case-fact case 
                                           (fire::make-isa o1 '?col)))))
           (o2-isas (mapcar #'(lambda (fact)
                                (third (third fact)))
                      (ltre::fetch-trues (fire::make-case-fact case 
                                           (fire::make-isa o2 '?col))))))
      (dolist (baseline-relation baseline-relations (cons vcr-context suggestions))
        (dolist (spec-relation (fire:all-specpreds baseline-relation reasoning-mt))
          (let ((arg1isas (qrg:trap-error (error nil)
                            (fire:arg-isa spec-relation 1)))
                (arg2isas (qrg:trap-error (error nil)
                            (fire:arg-isa spec-relation 2))))
            (when (and o1-isas o2-isas arg1isas arg2isas)
              ;; Bail if missing type information
              (let ((o1-support 
                     (remove-if-not 
                      #'(lambda (o1col)
                          (or (member o1col arg1isas :test #'equal)
                              (member o1col arg1isas 
                                      :test #'(lambda (c1 c2)
                                                (fire:spec-of? c1 c2 
                                                               :mt reasoning-mt)))))
                      o1-isas))
                    (o2-support
                     (remove-if-not 
                      #'(lambda (o2col)
                          (or (member o2col arg2isas :test #'equal)
                              (member o2col arg2isas 
                                      :test #'(lambda (c1 c2)
                                                (fire:spec-of? c1 c2 
                                                               :mt reasoning-mt)))))
                      o2-isas)))
                (when (and o1-support o2-support)
                  ;; Got one
                  (push (cons spec-relation 
                              (nconc
                               (mapcar #'(lambda (col)
                                           (fire::make-isa o1 col)) 
                                 o1-support)
                               (mapcar #'(lambda (col)
                                           (fire::make-isa o2 col)) 
                                 o2-support)))
                        suggestions))))))))))

(defun install-vcr-baseline-and-suggestions (case vcr-context-suggestions
                                              &key (reasoner fire::*reasoner*))
  ;; vcr-context-suggestions = (<vcr context> . <vcr suggestion>)
  ;; where <vcr context> =  ((o1 . g1)(o2 . g2) rcc8-fact baseline-relations)
  ;; and <vcr-suggestion> = (<relation> . <isa antecedents>)
  (let ((baseline-relations (fourth (car vcr-context-suggestions)))
        (o1 (caar (car vcr-context-suggestions)))
        (o2 (caadr (car vcr-context-suggestions)))
        (rcc8-fact (third (car vcr-context-suggestions)))
        (suggestions (cdr vcr-context-suggestions))
        (choices nil))
    (dolist (baseline-relation baseline-relations)
      (assert-fact reasoner 
        (fire:make-case-fact case
          (list baseline-relation o1 o2))
        (list (fire::contextualize-statement case rcc8-fact)) :vcr-baseline))
    (dolist (suggestion suggestions choices)
      (let* ((suggested-relation (car suggestion))
             (suggested-antecedents (cdr suggestion))
             (antes (mapcar #'(lambda (ante)
                                (fire::contextualize-statement case ante))
                      (cons rcc8-fact suggested-antecedents))))
        (assert-fact reasoner 
          (fire:make-case-fact case
            (list 'd::visualInterpretationRelationSuggestion suggested-relation 
                  o1 o2))
          antes :vcr-suggestion)
        (push (cons (list suggested-relation o1 o2) antes)
              ;; Is this the expected format for explanations?
              choices)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Suggestions from analogy

(defun gather-analogical-vcr-suggestions (subsketch-case reasoner)
  (fire::with-reasoner reasoner
    (let ((subsketch-name (car (fire::ask-it 
                                   `(d::subSketchGroupRepresentsObject
                                     ?bname ,subsketch-case)
                                 :context subsketch-case
                                 :response '?bname 
                                 :number 1))))
      (when (symbolp subsketch-name)
        (let* ((probe  ;;(d::ConceptualFactsOfCaseFn )
                `(d::NuSketchBundleCaseFn ,subsketch-name ,subsketch-case))
               (analogy-suggestions
                (fire::query `(d::analogySuggestionFor
                               (d::visualInterpretationRelationSuggestion
                                ?reln ?o1 ?o2)
                               ,probe ?mapping)
                  :context subsketch-case
                  :response '(?reln ?o1 ?o2))))
          analogy-suggestions)))))


;; our suggestions

(defun run-target-pipeline ()
  (setq case-lib-name 'NeuronCaseLibrary)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sorting the suggestions

;; 1) group suggestions & analogy suggestions by entity pairs
;; 2) remove any suggestions that appear in analogy suggestions as well
;; 3) mark any analogy suggestions that appear in the accepted list and remove from accepted
;; 4) if an accepted remains, remove it from suggestions
(defun sort-vcr-suggestions (subsketch-case suggestions analogy-suggestions 
                                            accepted)
  (mapcar
      #'(lambda (entities-suggestions)
          (multiple-value-bind (marked-analogy-suggestions other-accepted)
              (mark-vcr-analogy-suggestions-accepted 
               (first entities-suggestions) (third entities-suggestions) 
               accepted subsketch-case)
            `(,(first entities-suggestions)
              ,@(split-vcr-suggestions-by-truth
                 (remove other-accepted (second entities-suggestions)
                         :key 'car :test 'equal) 
                 subsketch-case)
              ,marked-analogy-suggestions
              ,other-accepted)))
      (filter-vcr-suggestions-by-analogy
       (sort-vcr-suggestions-by-entities suggestions analogy-suggestions))))

;; return a list of:
;; ((o1 o2) (suggestions) (analogy-suggestions))

(defun sort-vcr-suggestions-by-entities (suggestions analogy-suggestions)
  (let ((entities-table nil))
    (dolist (suggestion suggestions)
      ;; suggestion = ((?reln ?o1 ?o2) . <explanation>)
      (let* ((entities (cdar suggestion))
             (entry (assoc entities entities-table :test #'(lambda (x y)
                                                             (or (equal x y)
                                                                 (and (equal (car x) (cadr y))
                                                                      (equal (cadr x) (car y))))))))
        (unless entry
          (push (setq entry (list entities nil nil))
                entities-table))
        (push suggestion (second entry))))
    (dolist (suggestion analogy-suggestions)
      ;; analogy suggestion = ((?reln ?o1 ?o2) . <explanation>)
      ;;((analogySuggestionFor
      ;;                        (visualInterpretationRelationSuggestion ?reln ?o1 ?o2)
      ;;                        probe mapping) . <explanation>)
      (let* ((entities (cdar suggestion))
             (entry (assoc entities entities-table :test #'(lambda (x y)
                                                             (or (equal x y)
                                                                 (and (equal (car x) (cadr y))
                                                                      (equal (cadr x) (car y))))))))
        (unless entry
          (push (setq entry (list entities nil nil))
                entities-table))
        (push suggestion (third entry))))
    entities-table))

;; accepts a list of:
;; ((o1 o2) (suggestions) (analogy-suggestions))

(defun filter-vcr-suggestions-by-analogy (entities-table)
  (mapcar
   #'(lambda (entry)
       (list (car entry)
             (remove-if 
              #'(lambda (elt) (member elt (third entry) :key 'car :test 'equal)) 
              (second entry))
             (third entry)))
   entities-table))

;; accepts a entity pair, list of analogy suggestions of form:
;;  ((?reln ?o1 ?o2) . <explanation>)
;; and a list of:
;;  (?reln ?o1 ?o2)
;; returns muliptle values:
;;  list of (<analogy-suggestion> . accepted?)
;;  additional accepted value for this entity pair, if applicable

(defun mark-vcr-analogy-suggestions-accepted (entities analogy-suggestions 
                                                       accepted subsketch-case)
  (let ((accepted-these-entities (filter-analogy-accepted-by-entity entities accepted)))
    (values
     (mapcar
      #'(lambda (analogy-suggestion)
          (cons analogy-suggestion (not (null (member (car analogy-suggestion)
                                                      accepted-these-entities
                                                      :test 'equal)))))
      analogy-suggestions)
     (first ;; only one can be chosen
      (remove-if 
          #'(lambda (acceptance) 
              (accepted-fact-does-not-hold acceptance subsketch-case))
        (remove-if #'(lambda (elt) 
                       (member elt analogy-suggestions :key 'car :test 'equal))
          accepted-these-entities))))))

(defun filter-analogy-accepted-by-entity (entities accepted)
  (remove entities accepted :key 'cdr :test 
                 #'(lambda (x y) 
                     (not (or (equal x y)
                              (equal x (reverse y)))))))

;;; this is here because we don't want to display an accepted relationship
;;; that has been retracted
;; acceptance is of form
;;  (?reln ?o1 ?o2)

(defun accepted-fact-does-not-hold (acceptance subsketch-case)
  (null (fire::ask-it acceptance :context subsketch-case)))

(defun split-vcr-suggestions-by-truth (suggestions subsketch-case)
  ;; each suggestion is of the form (<reln> <o1> <o2> <source> <explanation>)
  ;; change the explanation for known facts
  (let ((known nil)
        (unknown nil))
    (dolist (suggestion suggestions)
      (let ((believed? (car (fire:ask-it (car suggestion)
                              :number 1
                              :context subsketch-case))))
        (cond (believed?
               (push (list (car suggestion) :ASK believed?) known))
              (t
               (push suggestion unknown)))))
    (list (sort unknown #'(lambda (x y) (alphalessp-by-field (car x) (car y))))
          (sort known #'(lambda (x y) (alphalessp-by-field (car x) (car y)))))))

(defun alphalessp-by-field (x y)
  (cond ((or (not (consp x))
             (not (consp y))) (ltre::alphalessp x y))
        ((ltre::alphalessp (car x) (car y)) t)
        ((ltre::alphalessp (car y) (car x)) nil)
        (t (alphalessp-by-field (cdr x) (cdr y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Response Handling

;;; user answers return bcase #reln #o1 #o2 accept-o1-o2-reln, retraction also 
;;; has #retract
(defun handle-visual->conceptual-questions (values reasoner)
  (let ((bcase (get-response-value "bcase" values)))
    (do ((i 0 (1+ i))
         (o1 nil (get-response-value (format nil "~Ao1" i) values))
         (o2 nil (get-response-value (format nil "~Ao2" i) values))
         (reln nil (get-response-value-str (format nil "~Areln" i) values))
         (analogy-believes nil (get-response-values (format nil "~Aanalogy" i) values))
         (retract nil (get-response-value (format nil "~Aretract" i) values)))
        ((and (not (= i 0)) (null o1)) nil)
      (unless (= i 0)
        (process-analogy-believes analogy-believes o1 o2 bcase reasoner)
        (cond ((not (null retract))
               (retract-user-selection reln o1 o2 bcase reasoner))
              (t
               (unless (string= reln "noreply")
                 (unless (relation-believed-by-user? o1 o2 reln bcase reasoner)
                   (assume-user-selection reln o1 o2 bcase reasoner)
                   (assert-selected-fact reln o1 o2 bcase reasoner)))))))))

(defun get-response-value (keystr values)
  (let ((str (cdr (assoc keystr values :test 'equal)))
        (*package* (find-package :data)))
    (and str (read-from-string str))))

(defun get-response-value-str (keystr values)
  (cdr (assoc keystr values :test 'equal)))

(defun get-response-values (keystr values)
  (do ((val-list nil (cons val val-list))
       (val (get-response-value-str (format nil "~A~A" keystr 0) values) 
            (get-response-value-str (format nil "~A~A" keystr i) values))
       (i 1 (1+ i)))
      ((null val) val-list)))

(defun gen-vcr-fact (reln o1 o2)
  (multiple-value-bind (forward? pred) 
      (parse-vcr-reln reln)
    (if forward?
        `(,pred ,o1 ,o2)
      `(,pred ,o2 ,o1))))

(defun user-acceptance-statement (reln o1 o2)
  `(d::userAcceptsBinaryRelationSuggestion ,@(gen-vcr-fact reln o1 o2)))

(defun assume-user-selection (reln o1 o2 bcase reasoner)
  (fire:tell-it (user-acceptance-statement reln o1 o2)
    :context bcase
    :reasoner reasoner
    :reason :user-vis-con-questions))

(defun assert-selected-fact (reln o1 o2 bcase reasoner)
  (let* ((vcr-fact (gen-vcr-fact reln o1 o2))
         (sketch (sketch reasoner))
         (subsketch (find-sketch-item bcase sketch))  ;; ***
         (glyph1 (find-sketch-item o1 subsketch))
         (glyph2 (find-sketch-item o2 subsketch)))
    (assert-sketch-fact reasoner :user-vis-con-questions
      bcase (list o1 o2) (list (user-acceptance-statement reln o1 o2)
                                 (last-modified-fact reasoner glyph1)
                                 (last-modified-fact reasoner glyph2))
      vcr-fact)
    (setf (modified? sketch) t)
    (publish-event (core sketch) :vcr-question-answered
      :sketch sketch
      :new-fact vcr-fact)
    vcr-fact))

(defun retract-user-selection (reln o1 o2 bcase reasoner)
  (let ((sketch (sketch reasoner))
        (vcr-fact (gen-vcr-fact reln o1 o2)))
    (setf (modified? sketch) t)
    (fire:untell-it (user-acceptance-statement reln o1 o2)
      :context bcase 
      :reasoner reasoner
      :reason :user-vis-con-questions)
    (publish-event (core sketch) :retract-vcr-answer
      :sketch sketch
      :old-fact vcr-fact)
    vcr-fact))

(defun relation-believed-by-user? (o1 o2 reln bcase reasoner)
  (not (null (car (fire::ask-it (user-acceptance-statement reln o1 o2)
                                :context bcase
                                :facts :wm
                                :reasoner reasoner)))))

(defun process-analogy-believes (believes o1 o2 bcase reasoner)
  (dolist (believe believes)
    (multiple-value-bind (truth-value reln)
        (parse-analogy-believe believe)
      (unless (null reln)
        (cond (truth-value
               (unless (relation-believed-by-user? o1 o2 reln bcase reasoner)
                   (assume-user-selection reln o1 o2 bcase reasoner)
                   (assert-selected-fact reln o1 o2 bcase reasoner)))
              (t
               (unless (not (relation-believed-by-user? o1 o2 reln bcase reasoner))
                 (retract-user-selection reln o1 o2 bcase reasoner))))))))

(defun parse-vcr-reln (stmt)
  (let ((*package* (find-package :data)))
    (values
     (unless (< (length stmt) 1)
       (string= (subseq stmt 0 1) "N"))
     (unless (< (length stmt) 2)
       (read-from-string (subseq stmt 1))))))

(defun parse-analogy-believe (stmt)
  (values
   (unless (< (length stmt) 1)
     (string= (subseq stmt 0 1) "T"))
   (unless (< (length stmt) 2)
     (subseq stmt 1))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code

