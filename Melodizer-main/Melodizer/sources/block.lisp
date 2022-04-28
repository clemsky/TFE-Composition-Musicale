(in-package :mldz)

;;;====================
;;;= BLOCK OBJECT =
;;;====================

(om::defclass! block ()
  ;attributes
  ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
   (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
   (bar-length :accessor bar-length :initform 0 :type integer)
   (beat-length :accessor beat-length :initform 0 :type integer)
   (voices :accessor voices :initform 12 :type integer)
   (style :accessor style :initform "None" :type string)
   (min-added-note :accessor min-added-note :initform nil :type integer)
   (max-added-note :accessor max-added-note :initform nil :type integer)
   (min-note-length :accessor min-note-length :initform nil :type integer)
   (max-note-length :accessor max-note-length :initform nil :type integer)
   (key-selection :accessor key-selection :initform nil :type string)
   (mode-selection :accessor mode-selection :initform nil :type string)
   (chord-key :accessor chord-key :initform nil :type string)
   (chord-quality :accessor chord-quality :initform nil :type string)
   (min-pitch :accessor min-pitch :initform 1 :type integer)
   (max-pitch :accessor max-pitch :initform 127 :type integer )
  )
  (:icon 225)
  (:documentation "This class implements Melodizer.
        Melodizer is a constraints based application aiming to improve composer's expression and exploration abilities
        by generating interesting and innovative melodies based on a set of constraints expressing musical rules.
        More information and a tutorial can be found at https://github.com/sprockeelsd/Melodizer")
)

(om::defclass! search ()
  ;attributes
  (
    (block-csp :accessor block-csp :initarg :block-csp :initform nil)
    (solution :accessor solution :initarg :solution :initform nil :documentation "The current solution of the CSP in the form of a voice object.")
    (result :accessor result
      :result :initform (list) :documentation
      "A temporary list holder to store the result of the call to the CSPs, shouldn't be touched.")
  )
  (:icon 225)
  (:documentation "This class implements Melodizer.
        Melodizer is a constraints based application aiming to improve composer's expression and exploration abilities
        by generating interesting and innovative melodies based on a set of constraints expressing musical rules.
        More information and a tutorial can be found at https://github.com/sprockeelsd/Melodizer")
)

; the editor for the object
(defclass block-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self block)) t)
(defmethod om::get-editor-class ((self block)) 'block-editor)

(defmethod om::om-draw-contents ((view block-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

; To access the melodizer object, (om::object self)

(defmethod initialize-instance ((self block-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

; function to create the tool's interface
(defmethod make-my-interface ((self block-editor))

  ; create the main view of the object
  (make-main-view self)

  ;;;;;;;;;;;;;;;;;
  ;;; sub views ;;;
  ;;;;;;;;;;;;;;;;;

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (block-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )


      ; part of the display for everything that has to do with adding new constraints to the problem
      (time-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 410 5)
        :bg-color om::*azulito*)
      )
      ; part of the display to put different solutions together
      (pitch-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 815 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-block-constraints-panel (make-block-constraints-panel self block-constraints-panel))

    (setf elements-time-constraints-panel (make-time-constraints-panel self time-constraints-panel))
    ; create the pitch constrains panel
    (setf elements-pitch-constraints-panel (make-pitch-constraints-panel self pitch-constraints-panel))
    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      block-constraints-panel
      time-constraints-panel
      pitch-constraints-panel
    )
  )
  ; return the editor
  self
)



    ;;;;;;;;;;;;;;;;;
    ;;; main view ;;;
    ;;;;;;;;;;;;;;;;;

; this function creates the elements for the main panel
(defun make-main-view (editor)
  ; background colour
  (om::om-set-bg-color editor om::*om-light-gray-color*) ;pour changer le bg color. om peut fabriquer sa propre couleur: (om-make-color r g b)
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the constraints panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-block-constraints-panel (editor block-constraints-panel)
  (om::om-add-subviews
    block-constraints-panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 120 20)
      "Block constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Bar length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 50)
      (om::om-make-point 200 20)
      "Bar length"
      :range '(0 1 2 3 4)
      :di-action #'(lambda (m)
        (setf (bar-length (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Beat length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Beat length"
      :range '(0 1 2 3)
      :di-action #'(lambda (m)
        (setf (bar-length (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Voices"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Voices"
      :range '(1 2 3 4 5 6 7 8 9 10 11 12)
      :di-action #'(lambda (m)
        (setf (voices (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Style"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 200)
      (om::om-make-point 200 20)
      "Style"
      :range '("None" "Full chords" "Arpeggio" "Hybrid")
      :di-action #'(lambda (m)
        (setf (style (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 250)
      (om::om-make-point 200 20)
      "Minimum added notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 250)
      (om::om-make-point 200 20)
      "Minimum added notes"
      :range (append '("None") (loop :for n :from 0 :upto 100 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (min-added-note (om::object editor)) 0)
          (setf (min-added-note (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 300)
      (om::om-make-point 200 20)
      "Maximum added notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 300)
      (om::om-make-point 200 20)
      "Maximum added notes"
      :range (append '("None") (loop :for n :from 100 :downto 0 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (max-added-note (om::object editor)) nil)
          (setf (max-added-note (om::object editor)) check))
      )
    )
  )


)

; this function creates the elements of the main additional constraints panel
; coordinates here are local to constraint-panel
(defun make-time-constraints-panel (editor time-constraints-panel)
  (om::om-add-subviews
    time-constraints-panel

    ; title
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 120 20)
      "Time constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Minimum note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 50)
      (om::om-make-point 200 20)
      "Minimum note length"
      :range (loop :for n :from 0 :upto 127 collect n)
      :di-action #'(lambda (m)
        (setf (min-note-length (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Maximum note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Maximum note length"
      :range (loop :for n :from 127 :downto 0 collect n)
      :di-action #'(lambda (m)
        (setf (max-note-length (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )
  )
)

(defun make-pitch-constraints-panel (editor pitch-constraints-panel)
  (om::om-add-subviews
    pitch-constraints-panel

    ; title
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 200 20)
      "Pitch constraints"
      :font om::*om-default-font1b*
    )

    ; Key

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Key selection"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 50)
      (om::om-make-point 200 20)
      "Key selection"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :di-action #'(lambda (m)
        (setf (key-selection (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    ; Mode
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Mode selection"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Mode selection"
      :range '("None" "ionian (major)" "dorian" "phrygian" "lydian" "mixolydian" "aeolian (natural minor)" "locrian" "pentatonic" "harmonic minor" "chromatic")
      :di-action #'(lambda (m)
        (setf (mode-selection (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Chord key"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :di-action #'(lambda (m)
        (setf (chord-key (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Chord quality"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 200)
      (om::om-make-point 200 20)
      "Chord quality"
      :range '("None" "Major" "Minor" "Augmented" "Diminished" "Major 7" "Minor 7" "Dominant 7" "Minor 7 flat 5" "Diminished 7" "Minor-major 7"
        "Major 9" "Minor 9" "9 Augmented 5" "9 flatted 5" "7 flat 9" "Augmented 9" "Minor 11" "Major 11" "Dominant 11" "Dominant # 11" "Major # 11")
      :di-action #'(lambda (m)
        (setf (chord-key (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 250)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 250)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :range (loop :for n :from 1 :upto 127 collect n)
      :di-action #'(lambda (m)
        (setf (min-pitch (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 300)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 300)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :range (loop :for n :from 127 :downto 1 collect n)
      :di-action #'(lambda (m)
        (setf (max-pitch (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

  )
)

; the editor for the object
(defclass search-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self search)) t)
(defmethod om::get-editor-class ((self search)) 'search-editor)

(defmethod om::om-draw-contents ((view search-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self search-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

; function to create the tool's interface
(defmethod make-my-interface ((self search-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      (search-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-search-panel (make-search-panel self search-panel))

    (om::om-add-subviews
      self
      search-panel
    )
  )
  self
)

(defun make-search-panel (editor search-panel)
  (om::om-add-subviews
    search-panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 120 20)
      "Search Parameters"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 15 50) ; position (horizontal, vertical)
      (om::om-make-point 130 20) ; size (horizontal, vertical)
      "Start"
      :di-action #'(lambda (b)
        (let init
          (setq init (new-melodizer (block-csp (om::object editor))))
          (setf (result (om::object editor)) init)
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 160 50) ; position
      (om::om-make-point 130 20) ; size
      "Next"
      :di-action #'(lambda (b)
        (if (typep (result (om::object editor)) 'null); if the problem is not initialized
          (error "The problem has not been initialized. Please set the input and press Start.")
        )
        (print "Searching for the next solution")
        ;reset the boolean because we want to continue the search
        (setf (stop-search (om::object editor)) nil)
        ;get the next solution
        (mp:process-run-function ; start a new thread for the execution of the next method
          "next thread" ; name of the thread, not necessary but useful for debugging
          nil ; process initialization keywords, not needed here
          (lambda () ; function to call
            (setf (solution (om::object editor)) (new-search-next (result (om::object editor)) (om::object editor)))
            (setf (om::tempo (solution (om::object editor))) (om::tempo (input-rhythm (om::object editor)))); set the tempo of the new voice object to be the same as the input
            (om::openeditorframe ; open a voice window displaying the solution
              (om::omNG-make-new-instance (solution (om::object editor)) "current solution")
            )
          )
        )
      )
    )
  )
)
