helloWorld();

outputString();

static void helloWorld()
{
    Console.WriteLine("Hello, World!");

    var username = Console.ReadLine();
    DateTime now = DateTime.Now;
    Console.WriteLine($"\nHello {username}!");
}

static void outputString()
{
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

