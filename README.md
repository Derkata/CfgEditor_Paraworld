# CfgEditor_Paraworld
Editor and parser for Settings.cfg and other Paraworld config files in the same json-like tree structured format


## Commands inside the application's terminal (for manual use), when the executable is called without arguments or double clicked:
* Set(path,value) - set argpath argvalue - sets the variable (or node) at the given path inside the tree structure to be equal to the given value
* Get(path) - get argpath - fetches and outputs the value of the variable (or node) at the given path
* Remove(path) - remove path - deletes the variable (or node and all of its subtree contents) at the given path
* Q - exits the application's terminal loop, ending execution
* CD path - changes work directory (work file) to given path - must be the full path to the file to be loaded for editing
* PWD - shows current working directory/path of currently loaded file
  
*The terminal loads Settings.cfg in path "...\AppData\Roaming\SpieleEntwicklungsKombinat\Paraworld" by default - use CD to change


## Commands, when the executable is called with arguments:
 * Get -> CfgEditor.exe -g path (filepath) | CfgEditor.exe --get path (filepath)
 * Remove -> CfgEditor.exe -r path (filepath) | CfgEditor.exe --remove path (filepath)
 * Set -> CfgEditor.exe -s path value (filepath) | CfgEditor.exe --set path value (filepath)

 *(filepath) arg is optional - if not given (filepath) is by default the path to Settings.cfg in "...\AppData\Roaming\SpieleEntwicklungsKombinat\Paraworld"

## Notes:
* Parses without whitespaces.
* Does not change the contents of the file, except those affected by set or remove.
* Automatically fixes the tabulation (formatting) inside the file.
* Commands in terminal: Not case sensitive;Not whitespace sensitive
* Errors exit with code 1 (Could't change it to have different code numbers)
* stderr prints descriptive errors
* If there is no Settings.cfg in AppData folder the application's terminal will ask you for a valid path
  until one is given.
* Problems in terminal with CD will just output error and ignore the command
* All illegal operations will change nothing
* The .hs file with the code is cursed open on your own risk
* Reference owner when using parts of the code or whole code or I steal your cat.
