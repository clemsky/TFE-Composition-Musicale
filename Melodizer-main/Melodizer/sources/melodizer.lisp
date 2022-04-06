(in-package :mldz)

;;;====================
;;;= MELODIZER OBJECT =
;;;====================

(om::defclass! melodizer ()
  ;attributes
  (
    (block-list :accessor block-list :initarg :block-list :initform '("Block 1 - Piece" "Block 2 - Verse" "Block 3 - Chorus") :documentation
    "The list of all the solutions saved by the user for the current search.")
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2") add a slot this way
  )
  (:icon 225)
  (:documentation "This class implements Melodizer.
        Melodizer is a constraints based application aiming to improve composer's expression and exploration abilities
        by generating interesting and innovative melodies based on a set of constraints expressing musical rules.
        More information and a tutorial can be found at https://github.com/sprockeelsd/Melodizer")
)


; the editor for the object
(defclass melodizer-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self melodizer)) t)
(defmethod om::get-editor-class ((self melodizer)) 'melodizer-editor)

(defmethod om::om-draw-contents ((view melodizer-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

; To access the melodizer object, (om::object self)

(defmethod initialize-instance ((self melodizer-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

; function to create the tool's interface
(defmethod make-my-interface ((self melodizer-editor))

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

      ; The coordinates here are coordinates in the main view

      ; part of the display for everything that has to do with input
      (block-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 300)
        :position (om::om-make-point 5 30)
        :bg-color om::*azulito*)
      )
      ; part of the display for everything that has to do with the search for solutions
      (search-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 300)
        :position (om::om-make-point 5 335)
        :bg-color om::*azulito*)
      )
      ; part of the display for everything that has to do with adding new constraints to the problem
      (time-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 410 30)
        :bg-color om::*azulito*)
      )
      ; part of the display to put different solutions together
      (pitch-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 815 30)
        :bg-color om::*azulito*)
      )
    )

    ; create the block panel
    (setf elements-block-panel (make-block-panel self block-panel))

    ; create the search panel
    (setf elements-search-panel (make-search-panel self search-panel))

    ; create the time constraints panel
    (setf elements-time-constraints-panel (make-time-constraints-panel self time-constraints-panel))

    ; create the pitch constrains panel
    (setf elements-pitch-constraints-panel (make-pitch-constraints-panel self pitch-constraints-panel))


    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      block-panel
      search-panel
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

  ; title
  (om::om-add-subviews
    editor
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 575 2) ; coin en haut a gauche de la zone de texte
      (om::om-make-point 100 30) ; dimensions de la zone de texte
      "Melodizer"
      :font om::*om-default-font3b*
    )
  )
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the input panel ;;;    final positions done
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this function creates all the elements of the input-panel (buttons, pop-up-menus,...)
; the coordinates are local to the input panel
(defun make-block-panel (editor block-panel)
  (om::om-add-subviews
    block-panel

    ; title
    (om::om-make-dialog-item
       'om::om-static-text
      (om::om-make-point 180 2)
      (om::om-make-point 120 20)
      "Blocks"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 10 30)
      (om::om-make-point 180 20)
      "Create Block"
      :di-action #'(lambda (b))
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 210 33)
      (om::om-make-point 180 20)
      "Block Selection"
      :range (block-list (om::object editor))
      :di-action #'(lambda (m))
    )
  )
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the search panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this function creates all the elements of the search-panel (buttons, pop-up-menus,...)
; coordinates here are local to search-panel
(defun make-search-panel (editor search-panel)
  (om::om-add-subviews
    search-panel

    ; title
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 180 2)
      (om::om-make-point 120 20)
      "Search"
      :font om::*om-default-font1b*
    )
  )
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the constraints panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  )
)
