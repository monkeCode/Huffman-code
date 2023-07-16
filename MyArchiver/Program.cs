using FSharp;
using Microsoft.FSharp.Collections;

string path = "your path";

var coding = path.Split('.').Last();


CodeFile(path, "result", coding);

void CodeFile(string path, string fileName, string extension)
{
    var filePath = string.Join("/", path.Split("/")[..^1]) + '/';

    var file = new StreamReader(File.Open(path, FileMode.Open, FileAccess.Read)).ReadToEnd();
    var res = FileCoding(file);
    var writer = new StreamWriter(File.Open(filePath+'/'+fileName+'.'+extension, FileMode.OpenOrCreate));
    writer.Write(res);
    writer.Flush();
}

void DecodeFile(string path, string fileName, string extension)
{
    var file = new StreamReader(File.Open(path, FileMode.Open, FileAccess.Read)).ReadToEnd();
    var res = FileDecoding(file);
    var writer = new StreamWriter(File.Open(fileName+extension, FileMode.OpenOrCreate));
    writer.Write(res);
    writer.Flush();
}

string FileCoding(string data)
{
    var (res, map) = HuffmanCode.Incode(data);
    var codingData = new List<string>();
    foreach (var kv in map)
    {
        codingData.Add((int)kv.Key +" " + kv.Value);
    }

    return string.Join("\n", codingData )+'\n' + res;
}

string FileDecoding(string data)
{
    var datas = data.Split('\n');
    var map = new Dictionary<char, string>();

    for (int i = 0; i < datas.Length-1; i++)
    {
        var d = datas[i].Split(" ");
        map.Add((char)int.Parse(d[0]), d[1]);
    }
    var codedData = datas[^1];
    return HuffmanCode.Decode(codedData, new FSharpMap<char, string>(map.Select(it => new Tuple<char, string>(it.Key,it.Value))));
}