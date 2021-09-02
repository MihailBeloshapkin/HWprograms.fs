module AvlTree
open System
open System.Collections
open System.Collections.Generic

/// <summary>
/// Binary tree.
/// </summary>
type Tree =
    | Node of value : int * height : int * left : Tree * right : Tree
    | Empty

/// <summary>
/// Binary search avl tree class and IEnumerator interface realization.
/// </summary>
type BinTree () =
    /// <summary>
    /// Binary tree.
    /// </summary>
    let mutable tree : Tree = Empty

    /// <summary>
    /// Current node.
    /// </summary>
    let mutable currentData : Tree = Empty

    /// <summary>
    /// Previous nodes.
    /// </summary>
    let mutable prevNodes : Stack<Tree> = Stack<Tree>()
    
    /// <summary>
    /// Visited vertexes.
    /// </summary>
    let mutable visited : List<int> = List<int>()

    /// <summary>
    /// Calculate hight of each node.
    /// </summary>
    member private this.ReCalculateHeight tree =
        let rec sub (t : Tree) =
            match t with
            | Node(value, height, Empty, Empty) -> (Node(value, 1, Empty, Empty), 1)
            | Node(value, height, left, right) -> let nodeHeightL = sub left
                                                  let nodeHeightR = sub right
                                                  let newHeight = Math.Max(nodeHeightL |> snd, nodeHeightR |> snd) + 1 
                                                  (Node(value, newHeight, nodeHeightL |> fst, nodeHeightR |> fst), newHeight)
            | Empty -> (Empty, 0)
        tree |> sub |> fst
    

    /// <summary>
    /// Add data to a binary tree.
    /// </summary>
    member this.Add (newData : int) =
        let rec subAdd (newData : int) (currentNode : Tree) =
            match currentNode with
            | Empty -> Node(newData, 1, Empty, Empty)
            | Node(value, height, left, right) -> if value < newData then 
                                                      Node(value, height, left, right |> subAdd  newData)
                                                  else
                                                      Node(value, height, left |> subAdd newData, right)
                                                
        
        tree <- tree |> subAdd newData
                     |> this.ReCalculateHeight   
                     |> this.Balance  
                     |> this.ReCalculateHeight


    /// <summary>
    /// Balance factor.
    /// </summary>
    member private this.BalanceFactor t =
        match t with
        | Node(_, _ , left, right) -> let mutable lHeight = -1
                                      let mutable rHeight = -1
                                      match left with
                                      | Empty -> lHeight <- 0
                                      | Node(_, height, _, _) -> lHeight <- height
                                      match right with
                                      | Empty -> rHeight <- 0
                                      | Node(_, height, _, _) -> rHeight <- height
                                      (rHeight - lHeight)
        | Empty -> 0

    /// <summary>
    /// Check that tree is balanced.
    /// </summary>
    member this.CheckThatBalanced () =
        let isBalanced t =
            match t with
            | Empty -> true
            | Node(_, _, left, right) -> let mutable lHeight = -1
                                         let mutable rHeight = -1
                                         match left with
                                         | Empty -> lHeight <- 0
                                         | Node(_, height, _, _) -> lHeight <- height
                                         match right with
                                         | Empty -> rHeight <- 0
                                         | Node(_, height, _, _) -> rHeight <- height
                                         Math.Abs(lHeight - rHeight) < 2
        let rec checker (tree : Tree) =
            match tree with   
            | Empty -> true
            | Node(value, height, left, right) -> if isBalanced tree then 
                                                      (checker left) && (checker right)
                                                  else false
        checker tree


    /// <summary>
    /// Left rotation of the vertex.
    /// </summary>
    member private this.LeftRotation t =
        let sub q =
            match q with
            | Node(qValue, qHeight, A, p) -> match p with
                                             | Node(pValue, pHeight, B, C) -> Node(pValue, pHeight, Tree.Node(qValue, qHeight, A, B), C)  
                                             | _ -> failwith "Incorrect node"
            | _ -> q
        sub t

    /// <summary>
    /// Right rotation of the vertex.
    /// </summary>
    member private this.RightRotation t =
        let sub p =
            match p with
            | Node(pValue, pHeight, q, C) -> match q with
                                             | Node(qValue, qHeight, A, B) -> Node(qValue, qHeight, A, Tree.Node(pValue, pHeight, B, C))
                                             | _ -> failwith "Incorrect node"
            | _ -> p
        sub t

    /// <summary>
    /// Get list of unbalanced nodes.
    /// </summary>
    member private this.GetList t =
        let rec lowestUnbalanced (tr : Tree) l =
            match tr with
            | Node(value, _, left, right) -> if tr |> this.BalanceFactor = 2 then
                                                 lowestUnbalanced right (value :: l)
                                             elif tr |> this.BalanceFactor = -2 then
                                                 lowestUnbalanced left (value :: l)
                                             else
                                                 (lowestUnbalanced left []) @ (lowestUnbalanced right []) @ l 
                                             
            | Empty -> l
        [] |> lowestUnbalanced t

    /// <summary>
    /// Balance tree in case of tree is not balanced.
    /// </summary>
    member private this.Balance t = 
        if this.GetList(t).IsEmpty then t else
            let lowestUnbalancedVertex = this.GetList(t) |> List.head
            let rec subBalancing t =
                match t with
                | Node(value, height, left, right) 
                    ->  if t |> this.BalanceFactor = 2 && value = lowestUnbalancedVertex then
                            if right |> this.BalanceFactor < 0 then
                                Node(value, height, left, this.RightRotation right) 
                                |> this.LeftRotation
                            else
                                Node(value, height, left, right) |> this.LeftRotation
                        elif t |> this.BalanceFactor = -2 && value = lowestUnbalancedVertex then
                            if left |> this.BalanceFactor > 0 then
                                    Node(value, height, this.LeftRotation left, right)
                                    |> this.RightRotation
                            else
                                 Node(value, height, left, right) |> this.RightRotation
                        else
                            if lowestUnbalancedVertex > value then 
                                Node(value, height, left, subBalancing right)
                            else
                                Node(value, height, subBalancing left, right)   
                | Empty -> Empty
                                                      
            subBalancing t

    /// <summary>
    /// Delete max value in sub tree and get it.
    /// </summary>
    member private this.DeleteMaxInSubTree (currentNode : Tree) =
        let mutable deletedData : int = 0
        let rec delete (current : Tree) : Tree =
            match current with
            | Node(value, height, left, Tree.Empty) -> deletedData <- value
                                                       Empty
            | Node(value, height, left, right) -> Node(value, height, left, delete right)
            | _ -> raise(ArgumentException("Sub tree")) 
        (deletedData, delete currentNode)

    /// <summary>
    /// Delete data from binary tree.
    /// </summary>
    member this.Delete (deleteData : int) =
        let rec subDelete (currentNode : Tree) =
            match currentNode with
            | Node(value, height, left, right) when value <> deleteData ->  if value < deleteData then
                                                                                Node(value, height, left, subDelete right)
                                                                            else
                                                                                Node(value, height, subDelete left, right)
            | Node(value, height, left, right) -> match left with
                                                  | Empty -> right
                                                  | _ -> let (maxInLeftSubTree, newLeft) = this.DeleteMaxInSubTree left
                                                         Node(maxInLeftSubTree, height, newLeft, right)
            | _ -> Empty
        tree <- tree |> subDelete
        tree <- tree |> this.ReCalculateHeight
                     |> this.Balance
                     |> this.ReCalculateHeight 
     
    /// <summary>
    /// Checks that current tree contains value.
    /// </summary>
    member this.Contains (sValue : int) =
        let rec searching (current : Tree) (searchVal : int) =
            match current with 
            | Node(value, _, left, right) -> if searchVal > value then
                                                 searching right searchVal
                                             elif searchVal < value then
                                                 searching left searchVal
                                             else
                                                 true
            | Empty -> false
        (tree, sValue) ||> searching

    
    /// <summary>
    /// IEnumerator realization.
    /// </summary>
    interface IEnumerator with
        /// <summary>
        /// Returns current vertex.
        /// </summary>
        member this.Current 
            with get () = match currentData with
                          | Empty -> match tree with
                                     | Empty -> raise(Exception()) 
                                     | Node(value, _, _, _) -> value :> obj 
                          | Node(value, _, _, _) -> value :> obj

        /// <summary>
        /// Reset numerator.
        /// </summary>
        member this.Reset () =
            currentData <- tree
            prevNodes.Clear()
            visited.Clear()

        /// <summary>
        /// Move to the next vertex.
        /// </summary>
        member this.MoveNext () =
            if currentData = Empty then 
                match tree with
                | Node(value, _, _, _) -> currentData <- tree 
                                          prevNodes.Push(currentData)
                                          visited.Add(value)
                                          true 
                | Empty -> false
            else 
                let rec getNext node =
                    match node with
                    | Node(value, height, Empty, Empty) -> getNext (prevNodes.Pop()) 
                    | Node(value, 
                           height, 
                           Node(lv, lg, ll, lr), 
                           Empty) -> if visited.Contains(lv) then
                                         getNext (prevNodes.Pop())
                                     else
                                         currentData <- Node(lv, lg, ll, lr)
                                         prevNodes.Push(currentData)
                                         visited.Add(lv)
                                         true
                    | Node(value, 
                           height,
                           Empty,
                           Node(rv, rh, rl, rr)) -> if visited.Contains(rv) then
                                                        getNext (prevNodes.Pop())
                                                    else
                                                        currentData <- Node(rv, rh, rl, rr)
                                                        prevNodes.Push(currentData)
                                                        visited.Add(rv)
                                                        true
                    | Node(value, 
                           heigth, 
                           Node(lv, lh, ll, lr), 
                           Node(rv, rh, rl, rr)) -> 
                                match visited.Contains(lv) && visited.Contains(rv) with
                                | true -> if prevNodes.Count > 0 then getNext (prevNodes.Pop()) else false
                                | _ ->  match visited.Contains(value) && prevNodes.Contains(node) |> not with
                                        | true -> prevNodes.Push(node)
                                        | _ -> ()  
                                        if visited.Contains(lv) |> not then
                                            currentData <- Node(lv, lh, ll, lr)
                                            prevNodes.Push(currentData)
                                            visited.Add(lv)
                                            true
                                        else if visited.Contains(rv) |> not then
                                            currentData <- Node(rv, rh, rl, rr)
                                            prevNodes.Push(currentData)
                                            visited.Add(rv)
                                            true
                                        else
                                            match prevNodes.Count with
                                            | 0 -> false
                                            | _ -> getNext (prevNodes.Pop())
                                                   
                    | _ -> false
                getNext currentData

