namespace Presentation
{
   using System;
   using System.Collections.Generic;
   using System.Diagnostics;
   using System.Linq;
   using System.Windows;
   using Microsoft.FSharp.Collections;
   using Microsoft.FSharp.Core;
   using static Parsing.FrqLogfileFormat;

   /// <summary>
   /// Interaction logic for App.xaml
   /// </summary>
   public partial class App
   {
      /// <summary>
      /// Handles the startup.
      /// </summary>
      /// <param name="sender">The sender.</param>
      /// <param name="e">The <see cref="System.Windows.StartupEventArgs"/> instance containing the event data.</param>
      private void HandleStartup(object sender, StartupEventArgs e)
      {
         var stopwatch = new Stopwatch();
         stopwatch.Start();

         FSharpChoice<FSharpList<LogEntry>, string> parsedContent = Parse("20160711_123012_PSReplayClient.log");

         if (parsedContent.IsChoice1Of2)
         {
            // ReSharper disable once PossibleNullReferenceException
            // Justification: checked above
            List<LogEntry> list = (parsedContent as FSharpChoice<FSharpList<LogEntry>, string>.Choice1Of2).Item.ToList();
         }

         stopwatch.Stop();
         Console.WriteLine("Time (ms):" + stopwatch.ElapsedMilliseconds);
      }
   }
}