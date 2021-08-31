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
/// Binary avl tree class and IEnumerator interface realization.
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
    /// Tree(used for testing, for example to check that tree is balanced).
    /// </summary>
    member this.Tree with get () = tree

    /// <summary>
    /// Calculate hight of each node.
    /// </summary>
    member private this.ReCalculateHight tree =
        let rec sub (t : Tree) =
            match t with
            | Node(value, height, Empty, Empty) -> Node(value, 1, Empty, Empty)
            | Node(value, height, left, right) -> let nodel = sub left
                                                  let noder = sub right
                                                  let mutable leftHeight = -1
                                                  let mutable rightHeight = -1
                                                  match nodel with
                                                  | Node(_, lHeight, _, _) -> leftHeight <- lHeight
                                                  | Empty -> leftHeight <- 0
                                                  match noder with
                                                  | Node(_, rHeight, _, _) -> rightHeight <- rHeight
                                                  | Empty -> rightHeight <- 0 
                                                  Node(value, Math.Max(leftHeight, rightHeight) + 1, nodel, noder)
            | Empty -> Empty
        sub tree
    

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
                     |> this.ReCalculateHight   
                     |> this.Balancing  
                     |> this.ReCalculateHight


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
    /// Left rotation of the vertex.
    /// </summary>
    member private this.LeftRotation t =
        let sub q =
            match q with
            | Node(qValue, qHeight, A, p) -> match p with
                                             | Node(pValue, pHeight, B, C) -> Node(pValue, pHeight, Tree.Node(qValue, qHeight, A, B), C)  
                                             | _ -> raise(Exception())
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
                                             | _ -> raise(Exception())
            | _ -> p
        sub t

    /// <summary>
    /// Balancing tree in case of tree is not balanced.
    /// </summary>
    member private this.Balancing t =
        let rec subBalancing t =
            match t with
            | Node(value, height, left, right) ->  if t |> this.BalanceFactor = 2 then
                                                       if right |> this.BalanceFactor < 0 then
                                                           Node(value, height, left, this.RightRotation right) 
                                                           |> this.LeftRotation
                                                        else
                                                            Node(value, height, left, right) |> this.LeftRotation
                                                    elif t |> this.BalanceFactor = -2 then
                                                        if left |> this.BalanceFactor > 0 then
                                                            Node(value, height, this.LeftRotation left, right)
                                                            |> this.RightRotation
                                                        else
                                                            Node(value, height, left, right) |> this.RightRotation
                                                    else
                                                        Node(value, height, subBalancing left, subBalancing right)  
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
        tree <- tree |> this.ReCalculateHight
                     |> this.Balancing
                     |> this.ReCalculateHight 
     
    /// <summary>
    /// Checks that current tree contains value.
    /// </summary>
    member this.Contains (sValue : int) =
        let rec searching (crnt : Tree) (searchVal : int) =
            match crnt with 
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
                           Node(rv, rh, rl, rr)) -> match visited.Contains(value) && prevNodes.Contains(node) |> not with
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
        


