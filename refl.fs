// <*--- coding = utf-8
module Rtool
open System
open System.Windows.Forms
open System.Reflection
open System.Collections.Generic
open System.IO
open System.Drawing

//open System.Net.Mime.MediaTypeNames
//System.IO.File.
//open System.Diagnostics
//System.IO.File.AppendText
//Debug.Print("start")
// type Logger(filename:string) as this=
//     [<DefaultValue>]    
//     val mutable file:TextWriter
//     do   this.file <- File.AppendText(filename) 
//     //new ()={filename =f}
//     member this.print (s:string)=
//         this.file.Write(s.ToCharArray())
//         this.file.FlushAsync()
//         ()

//let logger= new Logger("A:/loghu.log")

let ca (c:Control) i = c.Controls.Add(i)
let fm = new Form()

fm.Text <- "Class Reflection Tool"
fm.Font <- new Drawing.Font(new Drawing.FontFamily("Arial"),10.0f)

let tip1=new Label()
tip1.Text <- "namespace"
let namespace_cmb=new ComboBox()
let namespace_list = [| "System" ; "System.Windows" ; "System.Drawing" ;"System.Windows.Forms"  |]
namespace_cmb.Items.AddRange(namespace_list|> Array.map (fun x-> x:> obj ) )
let tip2 = new Label()
tip2.Text <- "Types"

let member_treeview= new TreeView()
let tip3= new Label()
tip3.Text <- "members:"

let type_treeView =new TreeView()


let sp = new SplitContainer()
sp.Dock <-DockStyle.Fill
sp.SplitterWidth <- 6
sp.Margin <- Padding(1)
ca fm sp
let lay2 = new TableLayoutPanel()
lay2.Dock<-DockStyle.Fill
ca sp.Panel2 lay2
//ca sp.Panel2 tip3
//tip3.Dock <- DockStyle.Top
//ca sp.Panel2 member_treeview

ca lay2 tip3
ca lay2 member_treeview
tip3.Dock <- DockStyle.Top
member_treeview.Dock <- DockStyle.Fill

let lay= new TableLayoutPanel()
lay.Dock <- DockStyle.Fill
lay.RowStyles.Add( RowStyle(SizeType.Absolute,25.0f)) |>ignore
lay.RowStyles.Add( RowStyle(SizeType.Absolute,25.0f)) |>ignore
lay.RowStyles.Add( RowStyle(SizeType.Absolute,25.0f)) |>ignore
lay.RowStyles.Add( RowStyle(SizeType.Percent,100.0f)) |>ignore
lay.ColumnStyles.Add( ColumnStyle(SizeType.Percent,100.0f)) |>ignore
//lay.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute,5.0f))
let pat c r i =   lay.Controls.Add(i,c,r); i.Dock <- DockStyle.Fill ; i.Margin <- Padding(1)
pat 0 0 tip1
pat 0 1 namespace_cmb
pat 0 2 tip2
pat 0 3 type_treeView
ca sp.Panel1 lay

fm.Size <- Drawing.Size(600,500)
fm.Show()

//let nsc= namespace_cmb
//namespace_cmb.SelectedIndex=0
let mutable currentAssembly=null
//////===========================================================================


let prettyTypeName (t:Type)=
    //let n=t.FullName.Replace(t.N)
    //n<- n.Replace(n+".","")
    let provider=CodeDom.Compiler.CodeDomProvider.CreateProvider("CSharp")
    let typeName=new CodeDom.CodeTypeReference(t.FullName.[(t.Namespace.Length+1)..])
    provider.GetTypeOutput(typeName)

let namespace_changed  e=
    let ns=namespace_cmb.Text
    let ass=System.Reflection.Assembly.Load(ns)
    if ass = null then
        MessageBox.Show("无法找到此命名空间程序集") |> ignore
        ()
    else
    currentAssembly <- ass
    let ts=ass.GetExportedTypes()
    let ns_list=[]
    let namespace_nodes =ResizeArray<TreeNode>()
    let root= type_treeView.Nodes

    for x in ts do
        let nameGroup= x.Namespace
        let ni=Seq.tryFindIndex (fun (x:TreeNode)-> x.Text=nameGroup) namespace_nodes
        let mutable i=0
        if(ni.IsNone) then
            let newSpaceNode= TreeNode(nameGroup)
            namespace_nodes.Add(newSpaceNode)
            newSpaceNode.ImageIndex <- 2
            i<- namespace_nodes.Count-1
        else
            i<- ni.Value
        let mutable showText=x.Name
        if(x.IsGenericType) then
            showText <- prettyTypeName x
        let nns=namespace_nodes.[i].Nodes.Add(showText)
        nns.Tag <- x.FullName
        let aa (x:Type)= 
            //FSharp.Reflection.FSharpType.IsUnion()
            match x with
                | _ when x.IsInterface -> 4
                | _ when x.IsAbstract ->  3
                | _ when x.IsClass -> 1
                | _ when x.IsNested ->3
                | _ when x.IsEnum -> 3
                | _ ->  7
        nns.ImageIndex<- aa x
    type_treeView.Nodes.Clear()
    type_treeView.Nodes.AddRange(namespace_nodes.ToArray())
    
    type_treeView.Sort()

let namespace_cmb'keyDown (e:KeyEventArgs) =
   if e.KeyValue = int( '\r') then
       namespace_changed e


//namespace_cmb.SelectedIndexChanged.Add(namespace_changed )

//namespace_cmb.KeyDown.Add( namespace_cmb'keyDown)

let type_selected e =
    let n=type_treeView.SelectedNode
    if n.Nodes.Count >0 then
        ()
    else
        //let ass =currentAssembly
        //let ammm= new  System.Diagnostics.Debug.

        // logger.print (sprintf "ass:%s\n" n.Tag :?> (t,loc) )
        // let ass=Assembly.Load(n.Parent.Text)
        // let t=ass.GetType(n.Tag :?> string) 
        let (fullname,location) = n.Tag :?>  (string *string) 
        //logger.print (sprintf "ass:%s\r\n---loc:%s\r\n" fullname location  )              
        let ass=Assembly.LoadFile(location)
        let t=ass.GetType(fullname)

        assert not (isNull t)
        let mems_raw=t.GetMembers()
        //let meths=t.getmethods()
        let props=t.GetProperties()
        let intfs=t.GetInterfaces()
        let evnts=t.GetEvents()
        let flds= t.GetFields()
        //let a = set[|1|]
        let inGroup aSeq a =
            aSeq |> Seq.exists (fun x->x=a)
        let mems= mems_raw |> Seq.map (fun (x:MemberInfo)->x.Name) |> SortedSet<string> 
        try
            let drop a (sets:SortedSet<'t>) = sets.Remove(a) |>ignore ; sets
            //props |> Seq.iter (fun x-> let n=x.Name in ( mems |> drop n |> drop ("get_" + n ) |> drop ("set_" + n) |> ignore ) )
            props |> Seq.iter (fun x-> let n=x.Name in ( [n;"get_"+n;"set_"+n]|> Seq.iter (mems.Remove >> ignore)  ) )
            //evnts |> Seq.iter (fun x-> let n=x.Name in ( mems |> drop n |> drop ("add_" + n ) |> drop ("remove_"+n) |> ignore ) )
            evnts |> Seq.iter (fun x-> let n=x.Name in ( [n;"add_" + n ;"remove_" + n] |> Seq.iter ( mems.Remove >> ignore ) ) )
        with
            | _-> MessageBox.Show("有不在member中的标识符，程序不正确") |> ignore
        let mems_out= mems |> Seq.map (fun x-> t.GetMember(x) )        
        let mt= member_treeview
        mt.Nodes.Clear()
        //printfn "members_count:%d" mems.Count
        if(mems.Count>0) then
            let member_node = new TreeNode("members (methods)")
            mems_out |> Seq.iter (fun (x:MemberInfo[])-> 
                let n= new TreeNode(let a=x.[0] in ( a.Name + " : " + a.ToString()) )
                member_node.Nodes.Add(n) |>ignore
                if(x.Length>1) then
                    x |> Seq.iter (fun (a:MemberInfo)->n.Nodes.Add(a.Name + " : " + a.ToString()) |>ignore )
            )
            mt.Nodes.Add(member_node) |> ignore
        //printfn "3members:%A" mems_out
        if(props.Length>0) then
            let prop_node = new TreeNode("property")
            props |> Array.iter (fun (x:PropertyInfo)-> prop_node.Nodes.Add(x.Name + " : " +  x.ToString())|> ignore ) 
            mt.Nodes.Add(prop_node) |> ignore
        if(evnts.Length>0) then
            let event_node = new TreeNode("event")
            evnts |> Array.iter (fun (x:EventInfo)-> event_node.Nodes.Add(x.Name + " : " + x.ToString())|> ignore ) 
            mt.Nodes.Add(event_node) |> ignore

        if(mt.Nodes.Count>0) then
            for i in mt.Nodes do
                i.Expand()
            mt.Sort()
            mt.Nodes.[0].EnsureVisible()

type_treeView.AfterSelect.Add(type_selected)
let imglist=new ImageList()
//new Drawing.Icon("313.ico")
//["colon.ico";"313.ico";"jade.ico"]
// let addicon (a:string) =
//     imglist.Images.Add(new Drawing.Icon(a))
["notitle1.ico";"notitle2.ico";"notitle3.ico";
    "notitle4.ico";"notitle5.ico";"notitle6.ico";
    "folder.ico";"tasks.ico";"313.ico";"jade.ico"] 
    |> Seq.iter (fun s-> new Drawing.Icon(s) |> imglist.Images.Add )
    //|> Seq.iter addicon

type_treeView.ImageList <- imglist

type IconIndex = Brace=2 | Interface =4 | Struct=6 | Class=1 | Enum_ =3 | Nested =3 | Generic=1
// last initial some work
//namespace_cmb.SelectedIndex=0    // not works
let groupNodes=ResizeArray<TreeNode>()
namespace_cmb.SelectedItem <- namespace_cmb.Items.[0]

let task1 =new System.Threading.Tasks.Task( fun () ->
    let exeDir=Application.ExecutablePath |> fun s-> s.[..(s.LastIndexOf('\\'))] 
    let mutable allType=[]    
    if(File.Exists(exeDir + "refl_tool.inf")) then
        // configure file all content needn't to confirm 
        let dlls=File.ReadAllLines(exeDir+"refl_tool.inf",Text.Encoding.UTF8)   
        //dlls |> Seq.iter (printfn "dlls:%s" )
        let asms= dlls |> Array.map (fun x-> Assembly.LoadFile(x))
        allType <- asms |>Array.fold (fun f a-> f @ 
                (a.GetExportedTypes() |> Array.map  (fun t->(t,a.Location)) |>List.ofArray )    
            ) []
    else
        let net4dir = @"C:\Windows\Microsoft.NET\Framework\v4.0.30319"
        let dlls= Directory.GetFiles(net4dir) |> Array.filter (fun x-> x.[(x.Length-3)..]="dll")  

        let asms=dlls |> Array.choose (fun x-> try Some(Assembly.LoadFile(x)) with   | _-> None )
        // let allType=
        //     asms 
        //     |> List.ofSeq 
        //     |> List.fold (fun x y->  x @  
        //         (List.ofArray(  y.GetExportedTypes() |> Array.map (fun t->(t,y.Location))  ) )
        //         )  []

        
        for asm in asms do
            let mutable tps =[||]
            try
                tps<-asm.GetExportedTypes()
            with
            | _ ->  ()
            if tps.Length=0 then
                ()
            else
                allType <- allType @ (tps|>List.ofArray |> List.map (fun x-> (x,asm.Location)) )
    let nameGroupTypes=allType |> List.groupBy (fun x-> (fst x).Namespace)
    for i  in nameGroupTypes do
        let ns:string=  fst i
        let groupNode= TreeNode(ns)
        groupNode.ImageIndex <- int IconIndex.Brace
        groupNodes.Add(groupNode)
        for (t,loc) in snd i do
            let mutable name =""
            if(t.IsGenericType) then
                name <-prettyTypeName(t)
            else
                name <- t.Name            
            let subNode= groupNode.Nodes.Add(name)
            subNode.Tag <- (t.FullName, loc)
            let iconInd = 
                match t with
                | _ when t.IsEnum -> IconIndex.Enum_
                | _ when t.IsGenericType -> IconIndex.Generic
                | _ when t.IsInterface -> IconIndex.Interface
                | _ when t.IsNested -> IconIndex.Nested
                | _ when t.IsValueType -> IconIndex.Struct
                | _ -> IconIndex.Class
            subNode.ImageIndex <- int iconInd
    groupNodes.Sort( Comparison(fun (a:TreeNode) (b:TreeNode) -> (compare a.Text b.Text) ) )
    //groupNodes.Sort()//(fun (a:TreeNode) (b:TreeNode) -> a.Text > b.Text ) )
    fm.Invoke(new MethodInvoker (fun ()->
        type_treeView.Nodes.AddRange(groupNodes.ToArray() )
        //type_treeView.
        ) ) |>ignore
    
    )
//dosome()
task1.Start()
let mutable appRunning=true
fm.Closing.Add(fun x-> (Application.Exit() ;Environment.Exit(0) ) )
//while appRunning do
//   Application.Run(fm)
[<STAThread>]
do 
    Application.Run(fm)