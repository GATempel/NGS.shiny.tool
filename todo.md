todo:

- [ ] imidiate toDo:
  - [ ] display tree and options

- [ ] dada script (functionality):
  - [ ] integrate the functionality to disable the script breaking occuring when the Kingdom determination is NA
  - [x] implement file.exists into taxonomy analysis for folder creation
  - [x] create new tables per taxa
  - [ ] illumina analysis
    - [x] implement skipping of tree to save time
    - [x] add the trunc left as a variable
  - [ ] taxonomy analysis
    - [x] implement choice of top X
    - [x] implement choice of fill for plot (Kingdom, Genus, Familiy)
    - [x] heatmap
      - [ ] taxa.order
      - [x] method choice
    - [x] phylogenetic tree
    - [x] abundance tables
    - [x] cooccurance
      - [x] use cooccur package
    - [ ] phylogenetic tree of A. thiox between samples
- [ ] shiny (usability):
  - [x] integrate skipping of tree option
  - [x] split data analysis and data visualisation into two different apps
  - [x] add reorder option for dada script application
  - [ ] add plot save format
  - [x] add in initial text in main panel to explain the app to the user
  - [x] app reactive inApp status reports for the user
  - [x] tabs
    - [x] session info
    - [x] tab for plots
  - [ ] estimated time of end
  - [x] systime
  - [ ] include a select all / deselect all option for the sample selection in the display app if possible
- other things
  - [ ] kontinuierliche Integration
  - [ ] markdown report
  - [ ] paketierung ! & dokumentation (roxygen)
    - [ ] use remotes package
  - [ ] recherchieren welche datenbank Lukas für die Taxonomy Zuordnung genutzt hat
 
priority:

- [ ] 1. functionality
- [ ] 2. usability via shiny
- [ ] 3. package creation and documentation
