helloWorld();

static void arrayUsage()
{
    // 宣言と初期化
    // 配列名 = new データ型[要素数];
    int[] numbers = new int[3];      // サイズ指定だけ
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    Console.WriteLine("=== Array ===");

    for (int i = 0; i < 2; i++)
    {
        Console.Write(numbers[i]);
        Console.Write(", ");
    }
    Console.WriteLine(numbers[2]);

    // 一括初期化
    // var 配列名 = new データ型[要素数]{ 初期値のリスト };
    // データ型[] 配列名 = { 初期値のリスト };
    string[] fruits = { "apple", "banana", "orange" };

    // 配列のサイズ
    Console.WriteLine(nameof(fruits) + ".Length: " + Int32.Parse(fruits.Length.ToString()));

    // 配列の要素をループで出力
    foreach (string fruit in fruits)
    {
        Console.WriteLine(fruit);
    }
    Console.WriteLine();

    Console.WriteLine("=== IEnumerable ===");
    // someWords is a string array.
    string[] someWords = {
        "the",
        "quick",
        "brown",
        "fox",
        "jumps"
    };

    string[] moreWords = {
        "over",
        "the",
        "lazy",
        "dog"
    };


    // Alphabetically sort the words.
    IEnumerable<string> query = from word in someWords
                                orderby word
                                select word;

    foreach (string str in query)
    {
        Console.WriteLine(str);
    }
}

arrayUsage();

outputString();

static void helloWorld()
{
    Console.WriteLine("Hello, World!");

    Console.Write("Input username> ");
    var username = Console.ReadLine();

    DateTime now = DateTime.Now;
    Console.WriteLine($"\nHello {username}!");
}

static void outputString()
{

    // Dictionaryの出力
    Dictionary<string, int> ages = new Dictionary<string, int>
        {
            { "Alice", 30 },
            { "Bob", 25 }
        };

    Console.WriteLine("=== Ages Dictionary ===");
    foreach (KeyValuePair<string, int> kvp in ages)
    {
        Console.WriteLine($"{kvp.Key} is {kvp.Value} years old.");
    }


}

