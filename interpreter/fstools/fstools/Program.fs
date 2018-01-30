// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let lines = System.IO.File.ReadLines(@"..\..\..\..\winmake.bat")
    for line in lines do
          let lastspace = line.LastIndexOf(" ")
          let out = line.Substring(lastspace+1,line.Length-lastspace-1)
          let out = out.Replace('/','\\')
          let wrap = "\n<Compile Include=\"..\..\{0}\" />"
          System.Console.WriteLine(wrap,out)

    System.Console.ReadLine()
    0 // return an integer exit code
