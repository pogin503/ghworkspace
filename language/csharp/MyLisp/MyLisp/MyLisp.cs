using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MyLisp
{
    enum ObjType
    {
        // Regular objects visible from the user
        TINT = 1,
        TCELL,
        TSYMBOL,
        TPRIMITIVE,
        TFUNCTION,
        TMACRO,
        TENV,
        // The marker that indicates the object has been moved to other location by GC. The new location
        // can be found at the forwarding pointer. Only the functions to do garbage collection set and
        // handle the object of this type. Other functions will never see the object of this type.
        TMOVED,
        // Const objects. They are statically allocated and will never be managed by GC.
        TTRUE,
        TNIL,
        TDOT,
        TCPAREN,
    }
    Dictionary<string, Obj> obarray = new Dictionary<string, Obj>;
    public class MyLisp
    {
        public MyLisp() {

        }
        public void repl()
        {
            while (true)
            {
                Console.Write("> ");
                Console.Write(eval(read()));
                Console.WriteLine();
                break;
            }

        }

        private void eval(string v)
        {
            throw new NotImplementedException();
        }

        private string read()
        {
            throw new NotImplementedException();
        }
    }
}
