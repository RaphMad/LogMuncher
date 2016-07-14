namespace Presentation
{
   using System.Diagnostics;
   using System.Linq;
   using Microsoft.FSharp.Collections;
   using Microsoft.FSharp.Core;
   using static Parsing.FrqLogfileFormat;

   /// <summary>
   /// Interaction logic for MainWindow.xaml
   /// </summary>
   public partial class MainWindow
   {
      public MainWindow()
      {
         InitializeComponent();
      }

      /// <summary>
      /// Handles the loaded.
      /// </summary>
      /// <param name="sender">The sender.</param>
      /// <param name="e">The <see cref="System.Windows.RoutedEventArgs"/> instance containing the event data.</param>
      private void HandleLoaded(object sender, System.Windows.RoutedEventArgs e)
      {
         var stopwatch = new Stopwatch();
         stopwatch.Start();

         FSharpChoice<FSharpList<LogEntry>, string> parsedContent = Parse("20160616_190033_lifeXCentral.log");

         if (parsedContent.IsChoice1Of2)
         {
            // ReSharper disable once PossibleNullReferenceException
            // Justification: checked above
            LogEntriesListView.ItemsSource = (parsedContent as FSharpChoice<FSharpList<LogEntry>, string>.Choice1Of2).Item.Where(x => x.HostId == "PDM-LXBMS-01").ToList();
         }
      }
   }
}