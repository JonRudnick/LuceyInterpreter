datatype BST = Empty 
| Node of BST * int * BST

fun insert item Empty = Node (Empty, item, Empty)
| insert item (Node (left, v, right)) = 
      if item < v then (Node (insert item left, v, right)) 
      else if item > v then (Node (left, v, insert item right)) 
      else (Node (left, v, right))
      
fun search item Empty = false
| search item (Node (left, v, right)) = if item = v then true 
    else if item > v then search item right else search item left