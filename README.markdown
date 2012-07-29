
# EEPY - Enhanced Emacs for PYthon
 Based on Gabriele Lanaro's **Emacs-for-Python(EPY)**  <https://github.com/gabrielelanaro/emacs-for-python>
 
 and **Python Development Emacs Environment (PDEE)** <https://github.com/pdee/pdee>

## Difference with emacs-for-python
eepy is based on [Gabriele Lanaro's Emacs-for-Python], with the following
improvments & changes: 

[Gabriele Lanaro's Emacs-for-Python]: https://github.com/gabrielelanaro/emacs-for-python

### disable ropemacs by default

 * for performance reason, disalbe ropemacs by default
 * autoload ropemacs when .ropeproject dir found on current dir or parent dir

### improve code completion

 * pycompletemine from [pdee], plus the following changes
     - made it a source for auto-complete
     - made it work on python-shell
 * a new auto-complete source for GNU Emacs's built-in python completion
 * a new auto-complete source with external API files (SciTE way)
 * [ ] auto-complete source for ipython (only python-mode.el supported). Stolen from
   <https://bitbucket.org/tavisrudd/emacs.d/src/tip/dss-completion.el>

[pdee]: https://github.com/pdee/pdee/

### improve flymake

 * eepy uses this fork version of flymake.el: <https://github.com/illusori/emacs-flymake>
     - Support for queuing up syntax checks once a certain number are in-progress.
     - Support for placing temporary files in the system temporary directory.
     - Spawns only one buffer-modification timer check rather than one per buffer.
     - Show multiple errors in tooltips.
     - Improved support for remote files over Tramp.
     - Improved error message classification. (err/warn/info, customizable regexp)
     - Support for new languages: Javascript and CSS.
     - Minor other bug fixes.
 * eepy's changes
     - Python package pep8 & pylint already bundled in this suite. You don't need to install them by yourself.
     - Use flymake-cursor.el to displays flymake msg in minibuffer when cursor moves to corresponding position
     - You can switch flymaker within between epylint/pep8/pyflakes/pychecker when editing. (not only before eepy start-up)
     - You can invoke flymaker manually with eepy-flymake-with

### python documentation

 * chm
     - keyhh on windows
 * pycompletemine's sub-features
     - `py-complete-help-thing-at-point`(<M-f1>)
     - `py-complete-signature-expr`(<M-f2>)
     - `py-complete-help`(<M-f3>)
 * `info-lookup-symbol`(C-h S): query python document in texinfo format (stolen from `info-lookup` from [loveshack's python.el])
     - [X] python documentation in info format (not available in python 2.6/2.7's official packages)
     - [X] or, [pydoc-info] ?
 * `pydoc' command line tool

 [loveshack's python.el]: http://www.loveshack.ukfsn.org/emacs/
 [pydoc-info]: https://bitbucket.org/jonwaltman/pydoc-info/


### support different python major modes

 * emacs 23 & 24's built-in python-mode
 * [python-mode.el from launchpad.net]
 * [fgallina's python.el]
 * [loveshack's python.el]

 [python-mode.el from launchpad.net]: https://launchpad.net/python-mode
 [fgallina's python.el]: https://github.com/fgallina/python.el
 [loveshack's python.el]: http://www.loveshack.ukfsn.org/emacs/python.el


### others features

 * imenu integration
     - choose method (M-x imenu)
     - anything-imenu / anything-browse-code
     - imenu tree
 * code folding with visual indicators
     - hideshow + hideshowvis
     - outline + outline  
 * [ ] develock to highlight code style violations


### add an easy-to-use menu, you can invoke most stuff here

 * toggle auto-complete sources
 * turn on/off flymaker, and switch flymaker between pylint/pep8/pyflakes/pychecker
 * invoke syntax checking with pylint/pep8/pyflakes/pychecker 


### remove Gabriele Lanaro's personal preferences

 * disabled autopair-mode by default
 * disabled smart-operator-mode by default 
 * remove epy's configurations file epy-bindings, epy-editing

- - -
## Quick Start

### Quick Start
1\. In your dotemacs, add path of `eepy' to `load-path'

    (add-to-list 'load-path "/some/where/storing/eepy")
    
2\. On Linux, make sure all files in folder `bin' have x permission

    chmod u+x /some/where/storing/eepy/bin/*    
  
   (On Windows, you don't need to do anything).

3\. Load feature `eepy', either in dotemacs

    (require 'eepy)

   or on demand:

    M-x load-library RET eepy RET
   
4\. Then open your python source files to edit. You can access most features from the `eepy' menu.

### If you want to customize something

Just customize eepy group.
    M-x customize-group RET eepy RET

Or, you can edit `eepy-start.el` to suit your needs, and then load `eepy-custom` rather than `eepy`.

- - -
## Limitations & TODO

 * Only tested on GUI version of Emacs 23.1 & 24.0.95, on Windows XP SP2 & LinuxMint 11.
     - [ ] test on console version of Emacs, esp. fringe stuff (code folding, flymake etc)
         - [ ] qtmstr-outline works fine under term mode, hideshowvis would cause hanging
 * Only tested on CPython 2.6 & 2.7
     - [ ] Currently on plan for CPython 3.x or Jython/IronPython
 * [ ] Test virtualenv feature of EPY
 * IPython shell only works under python-mode.el.
     - [ ] Make it work on other python major modes (possible?)
     - [ ] Workaround for a problem on Windows (python -i)
     - [ ] ipython.el not compatible with python-mode.el>6.0.2
 * [ ] Project support
     - epy-0.3 uses eproject, but it's better for file-based project,
       not good for folder-based project (in which case, user shouldn't
       need to maintain the file list)
     - folder-based project
       - [ ] eproject (jrockway)
       - [ ] projectile 
 * [ ] Yasnippet not tested
 * python documentation
     - [ ] [pylookup] (not yet)
     - [ ] pydoc-info
     - [X] pydoc command line?
     - [ ] would keyhh work on wine? (low priorty)
 * [X] Make stacktrace lines clickable (in eshell)


 [pylookup]: https://github.com/tsgates/pylookup

