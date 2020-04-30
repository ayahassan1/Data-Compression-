using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
namespace DLLTestSampleCSharp
{
    class Program


    {

        [DllImport("Project.dll")]
        private static extern void Compression();
        [DllImport("Project.dll")]
        private static extern void original_text();

        
        static void Main(string[] args)
        {
            try
            {

               
                Compression();
                original_text();
               // Console.WriteLine();

               
            }
            catch (Exception e)
            {
                e.GetType();
            }
        }
    }
}
