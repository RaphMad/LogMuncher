namespace Presentation
{
   using System.Linq;
   using System.Windows;
   using Parsing;

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
         var test = FrqLogfileFormat.ParseV1("input.txt");

         var success = test.Where(x => x.IsChoice1Of2).ToArray();
         var error = test.Where(x => x.IsChoice2Of2).ToArray();
      }
   }
}