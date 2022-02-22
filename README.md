# vowel-plotter

## Interactive vowel plotter using ggplot2 and shiny

Upload a spreadsheet of vowel formant measurements in `.csv` format and this tool will generate vowel plots made using `ggplot2`.

By default, the plotter tool will look for columns named *F1* and *F2* for your formant measurements, *vowel* for the vowel category labels, and *word* for the word labels, so it works best if your spreadsheet contains these column names. It will still work if your columns are labelled differently though - you just need to use the drop-down menus to specify which columns contain the various labels/values needed to plot.

Developed by [George Bailey](https://www.gbailey.uk/) (2022) at the University of York

![Screenshot of the vowel plotter tool](/www/demo_screenshot.png)