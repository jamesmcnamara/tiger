structure Tree = struct
    datatype 'a tree = Tree of ('a * 'a tree * 'a tree) | Leaf

    fun insert cmp t elem =
        case t of
            Tree (x, l, r) => if cmp x elem
                              then Tree (x, (insert cmp l elem), r)
                              else Tree (x, l, (insert cmp r elem))
          | Leaf          => Tree (elem, Leaf, Leaf)
  
    fun from_list cmp l =
        case l of
            x::xs => insert cmp (from_list cmp xs) x
          | []    => Leaf
end
