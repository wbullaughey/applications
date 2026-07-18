function numberRows (sheet,column) {
  var columnValues = sheet.getRange(column+':'+column).getValues(); // Get all values in column
  return columnValues.filter(row => row[0] !== '').length; // Count non-empty cells
}
function copyCurrent() {
  // Get the active spreadsheet
  var spreadsheet = SpreadsheetApp.getActiveSpreadsheet();

  // Optional: Log the spreadsheet name to confirm
  Logger.log("Current spreadsheet: " + spreadsheet.getName());

  // Optional: Get the active sheet (the currently selected tab)
  var sheet = spreadsheet.getSheetByName("Collection");
  Logger.log("Active sheet: " + sheet.getName());
  var date = sheet.getRange("A1").getValue();
  var ui = SpreadsheetApp.getUi();
  if (date == ""){
    ui.alert ("service date not set");
    return;
  }
  var total=sheet.getRange("K6").getValue();
  if (total == 0){
   ui.alert ("no income recorded");
    return;
  }
  var year = Utilities.formatDate(new Date(date), Session.getScriptTimeZone(), "YYYY");
  var month = Utilities.formatDate(new Date(date), Session.getScriptTimeZone(), "MMMM");
  Logger.log("year: " + year +" date: "+date + " month " + month);
  switch(month.length){
    case 4:
      break;
    default:
      month = month.slice(0,3);
  }
  Logger.log(" month " + month);
  var tab = month + " " + year;
  Logger.log(" tab " + tab);
  // Get the data from the source sheet
   // Get the dimensions of the source data
  var sourceColumns = sheet.getLastColumn();
  var sourceRows = Math.max(numberRows (sheet,'B'),numberRows (sheet, 'F'));
  var sourceRange = sheet.getRange(1,1,sourceRows,sourceColumns);
  var sourceData = sourceRange.getValues();
  Logger.log("rows " + sourceRows + " columns " + sourceColumns);
  var targetSheet = spreadsheet.getSheetByName(tab);
  if (targetSheet == null){
    targetSheet = spreadsheet.insertSheet(tab);
     targetSheet.setName(tab);
     // Move the sheet to the desired index
    //targetSheet.setActiveSheet;
    //targetSheet.moveActiveSheet(2);
    var targetRange = targetSheet.getRange(1,1,sourceRows,sourceColumns);
  } else {
    // Find the last column with data in the target sheet
    var lastRow = targetSheet.getLastRow() + 2;
    var targetRange = targetSheet.getRange(lastRow,1,sourceRows,sourceColumns);
    Logger.log("target: "+ targetRange.getRow() + "," + targetRange.getColumn() + ":" + targetRange.getLastRow() + "," + targetRange.getLastColumn());
  }
  // Define the target range (starting at column after lastCol, row 1)

  // Write the source data to the target sheet
  targetRange.setValues(sourceData);
  // clear the current collection
  sheet.getRange("C3:C1000").clear();
  sheet.getRange("F3:F1000").clear();
  sheet.getRange("G3:G1000").clear();
  sheet.getRange("H3:H1000").clear();
 sheet.getRange("A1:A1").clear();
 }
 function getValue(cellReference){
  var spreadsheet = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = spreadsheet.getSheetByName("Collection");
  return sheet.getRange(cellReference).getValue();
 }
 function printCollection(){
  var date = Utilities.formatDate(new Date( getValue("A1")), Session.getScriptTimeZone(), "MM/dd/YYYY");
  var title = "Sunday Collection"
  var counter = getValue("K2");

  var cash = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(getValue("K4"));

  var checks = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(getValue("K5"));

  var total = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(getValue("K6"));

  var html = HtmlService.createHtmlOutputFromFile('Collection')
      .setWidth(400)
      .setHeight(300);
  // Pass parameters via a client-side script instead of appending to HTML
    html.setTitle(title);
    var script = `<script>
    var params = {
      date: "${date}",
      counter: "${counter}",
      cash: "${cash}",
      checks: "${checks}",
      total: "${total}"
    };
    document.addEventListener('DOMContentLoaded', function() {
      document.getElementById('date').innerText = params.date;
      document.getElementById('counter').innerText = params.counter;
      document.getElementById('cash').innerText = params.cash;
      document.getElementById('checks').innerText = params.checks;
      document.getElementById('total').innerText = params.total;
    });
  </script>`;
  // Pass parameters to the HTML using a client-side script
   html.append(script); // Append script to set parameters
  SpreadsheetApp.getUi().showModalDialog(html, title);
 }
 function printDeposit(){
  var date = Utilities.formatDate(new Date( getValue("A1")), Session.getScriptTimeZone(), "MM/dd/YYYY");
  var title = "S&T Deposit"

  var cash = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(getValue("D3"));

  var checks = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(getValue("P6"));

  var total = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(getValue("P7"));

  var html = HtmlService.createHtmlOutputFromFile('Deposit')
      .setWidth(400)
      .setHeight(300);
  // Pass parameters via a client-side script instead of appending to HTML
    html.setTitle(title);
    var script = `<script>
    var params = {
      date: "${date}",
      cash: "${cash}",
      checks: "${checks}",
      total: "${total}"
    };
    document.addEventListener('DOMContentLoaded', function() {
      document.getElementById('date').innerText = params.date;
      document.getElementById('cash').innerText = params.cash;
      document.getElementById('checks').innerText = params.checks;
      document.getElementById('total').innerText = params.total;
    });
  </script>`;
  // Pass parameters to the HTML using a client-side script
   html.append(script); // Append script to set parameters
  SpreadsheetApp.getUi().showModalDialog(html, title);
 }

 function onOpen() {
  var ui = SpreadsheetApp.getUi();
  ui.createMenu('Collections Menu')
    .addItem('Archive Collection', 'copyCurrent')
    .addItem('Print Collection', 'printCollection')
    .addItem('Print Deposit', 'printDeposit')
    .addToUi();
}
function test() {
 copyCurrent();
  // printCollection();

}
