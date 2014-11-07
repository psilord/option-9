(in-package :option-9)


(defmethod add-child ((parent frame) (child frame))
  "Return the CHILD after it has been added to PARENT. No checks for
duplicate adds are performed."
  (unless (children parent)
    (setf (children parent) (make-hash-table :test #'eq)))

  (setf (gethash child (children parent)) child)
  (setf (parent child) parent)
  child)

;; This is recursive from the root (at the parent) to the leaves, but
;; I don't ever expect the depth of this tree to be stack breaking.
;; When the child is removed, all children of it are ripped out of the
;; scene tree with it too. XXX Fix using the orphan-policy mechanism.
(defmethod remove-child ((parent frame) (child frame))
  "Return CHILD if removed, or NIL otherwise."
  (when (children parent)
    (if (remhash child (children parent))
        ;; XXX If this child had children of its own, where do they go?
        ;; SHould they just get shoved into the universe and be done with them?
        child
        ;; Walk all children subtrees recursively until I find it, then bail.
        (loop for p being the hash-values in (children parent) do
             (when (remove-child p child)
               ;; Child currently has no parent.
               (setf (parent child) nil)
               (return-from remove-child child))))))


(defgeneric insert-into-scene (scene-manager child-drawable parent-drawable)
  (:documentation "Insert the CHILD-FRAME object as a child of the
specified PARENT-FRAME and then insert it into the specified view. If
PARENT-FRAME is :universe, then insert the object as a child of the
universe frame. Return the PARENT-FRAME and CHILD-FRAME as values, in
that order."))

(defgeneric remove-from-scene (scene-manager the-drawable)
  (:documentation "Remove THE-FRAME (and by consequence all of its
children) from the scene-db and return THE-FRAME. If it is desired that
the children of THE-FRAME should participate in the scene, then it is
up to you to modify their bases to be in the right coordinate frame
along with their other physcal vectors and such, and reinsert them."))


(defmethod insert-into-scene ((sman scene-manager) (child drawable)
                              parent)

  ;; We poke in the child into the scene-tree
  (add-child (if (eq parent :universe) (root sman) parent) child)

  ;; Add the item into each role view it specifies.
  ;; Currently, it is stored in a list. I may change this to be a hash table
  ;; whose key is the child and value is T.
  (dolist (role (roles child))
    (push child (gethash role (views sman)))))

;; Return a list of the entities that have the role specified.
(defmethod entities-with-role ((sman scene-manager) role)
  (gethash role (views sman)))

;; Return a list that collects all the entities in the roles specified.
(defmethod all-entities-in-roles ((sman scene-manager) &rest roles)

  (let ((entity-lists (mapcar #'(lambda (role)
                                  (entities-with-role sman role))
                              roles)))
    ;; NIL entity lists are squeezed out by the appending keyword...
    (loop for group in entity-lists appending group)))

;; TODO: This is broken and doesn't understand the orphan policy! It
;; just the item and all children recursively.
(defmethod remove-from-scene ((sman scene-manager) item)
  ;; 1. remove the item out of all of the role views.  If this starts
  ;; sucking in speed as the number of objects rise, then change it to
  ;; be removing stuff out of a hash table.
  (dolist (role (roles item))
    (symbol-macrolet ((the-view (gethash role (views sman))))
      (setf the-view (remove-if #'(lambda (x) (eq x item)) the-view))))

  ;; 2. get the parent of the item from the var and then remove-child it.
  (remove-child (parent item) item)

  ;; 3. TODO This needs to be verified that it is right.
  (when (children item)
    (loop for child being the hash-values in (children item) do
         (remove-from-scene sman child))))
