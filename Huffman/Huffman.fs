module HuffmanCode

[<AllowNullLiteralAttribute>]
type private Node = class 

    val mutable value:char
    val mutable count:int

    [<DefaultValue>]
    val mutable left:Node

    [<DefaultValue>]
    val mutable right:Node

     new(_value:char, _count:int) = {value = _value;count = _count}

    
     member public this.Increment() = this.count <- 1 + this.count

end

let private AddOrIncrement(elem:char) (nodes: Node List) : Node List =
    
    let foundItem = nodes |> List.tryFind (fun item -> item.value = elem)

    if foundItem.IsSome then
        foundItem.Value.Increment()
        nodes
    else
        let insetring =  Node(elem, 1)
        let list = List.insertAt nodes.Length insetring nodes
        list
    
let rec private MakeTree (nodes: Node List):Node = 
    if nodes.Length = 1 then
        nodes[0]
    else
        let newNodes = nodes[2..]
        let newNode = Node('e',nodes[0].count + nodes[1].count)
        newNode.left <- nodes[0]
        newNode.right <- nodes[1]
        let i = newNodes |> List.tryFindIndex(fun it -> it.count > newNode.count)
        let mutable index = 0
        if i.IsNone then
            index <- newNodes.Length
        else
            index <- i.Value
        MakeTree(newNodes|> List.insertAt index newNode)
   
let rec private CollectMap(tree:Node) (map:Map<char,string>) (code:string):Map<char,string> = 
    if isNull tree.left &&  isNull tree.right then
        map.Add(tree.value, code)
    else
        let mutable m = map
        if not (isNull tree.left) then
            let n = tree.left
            m <- CollectMap n m (code + "0")
        if not (isNull tree.right) then
            m <- CollectMap tree.right m (code + "1")
        m

let private GetCodeMap (tree:Node) = 
    let mutable m = Map[]
    CollectMap tree m ""


let public Incode datas = 
    let mutable nodes = []
    for el in datas do 
        nodes <- AddOrIncrement el nodes
    nodes <- nodes |> List.sortBy (fun it -> it.count)  
    
    let tree = MakeTree nodes
    let m = GetCodeMap tree
    let mutable newData = ""
    for ch in datas do
         newData <- newData + m[ch]
    (newData, m)

let public Decode (datas:string) (map:Map<char, string>) = 
    let mutable result = []
    let mutable curIndex = 0
    let mutable offset = 0
    let kvs = [for i in map.Values -> i]
    while curIndex + offset < datas.Length do
        let sub = datas[curIndex..curIndex+offset]
        let finds = List.filter (fun (it:string) -> it = sub) kvs
        if finds.Length =1 then
            result <- result|> List.append finds
            curIndex <- curIndex + offset+1
            offset <- 0
        else
            offset <- offset + 1
    result |> List.map (fun it -> 
        let mutable r = char(0)
        for kv in map do 
            if kv.Value = it then
                r <- kv.Key
        r.ToString())|> List.rev |> String.concat ""