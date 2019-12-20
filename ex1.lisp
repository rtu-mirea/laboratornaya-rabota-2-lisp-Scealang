(defun test_list ()
    `(3, 9, 2, 15, 5, 8, 34, 1, 99, 20)
)


;; Функция вставки элемента в список
(defun _insert_element (head tail at value current)
    (if (= at current)
        (append head (cons value tail))
        (_insert_element 
            (append head (list (car tail)))
            (cdr tail)
            at
            value 
            (+ 1 current )
        )
    )
)


;; Функция удаления элемента из списка
(defun _remove_element (head tail at current)
    (if (= at current)
        (append head (cdr tail))
        (_remove_element 
            (append head (list (car tail)))
            (cdr tail)
            at
            (+ 1 current )
        )
    )
)


;; Функция поиска элемента списка
(defun _index_element (value lst index)
    (if (= (car lst) value)
        index
        (_index_element value (cdr lst) (+ 1 index))
    )
)

(defun insert_el (lst at value)
    (_insert_element `() lst at value 0)
)

(defun remove_el (index lst)
    (_remove_element `() lst index 0)
)

(defun index_el (value lst)
    (_index_element value lst 0)
)

(insert_el (test_list) 2 3131)
(index_el 5 (test_list))
(remove_el 3 (test_list))
