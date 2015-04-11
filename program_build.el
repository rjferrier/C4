;; ================
;; F90-UNIT-TESTING
;; ================
;;
;; Source and unit test code preprocessing.  Contains various functions
;; for converting Fortran source code into pFUnit-based modules and
;; programs.  The functions also generate a top-level Makefile based on
;; automatically generated dependencies at leaf levels for both
;; the src and test directory trees.
;;
;; R.J.Ferrier 
;; 18/05/2011

;; === SETTINGS ===

;; for debugging
(setq VERBOSE_MODSCAN nil)
(setq VERBOSE_DEPENDENCIES nil)
(setq VERBOSE_RECURSION nil)
(setq VERBOSE_WRAPPING nil)
(setq VERBOSE_RULES nil)
(setq VERBOSE_NAVIGATION nil)


;; the trunk directory is assumed to exist within the project directory
(setq F90_TRUNK_NAME "trunk_f90/")
(setq F90_TRUNK_DIR (concat REPOS_DIR F90_TRUNK_NAME))
(setq F90_BUILD_DIR (concat F90_TRUNK_DIR "build/"))
(setq F90_EXE_NAME "tests.x")

(setq CPP_TRUNK_NAME "trunk_cpp/")
(setq CPP_TRUNK_DIR (concat REPOS_DIR CPP_TRUNK_NAME))
(setq CPP_BUILD_DIR (concat CPP_TRUNK_DIR "test2D/build/"))
(setq CPP_EXE_NAME "C4_2DTests")

;; a convention: in the source code, module names may be suffixed to
;; avoid clashes with type and instance names.  But at the directory
;; level, corresponding file or folder names counterparts are not
;; suffixed because it would be unnecessary.
(setq FOLDER_SUFFIX "Folder")
(setq MODULE_SUFFIX "Module")
(setq MODULE_PREFIX "m_")

;; some arbitrary names
(setq DEPENDENCIES_FILENAME "rules.mk")
(setq DIRECTIVES_FILENAME "compiler.mk")
(setq REGEN_TAG "@REGEN")

;; if the following is set to t, test procedures will always invoke setUp()
;; and tearDown() routines if they are available in the same module.
;; Otherwise f90 test procedures must have a fixture argument in order to
;; invoke setUp(fixture) and tearDown(fixture).
(setq NO_FIXTURE_ARGS t)

;; m4 stuff
(setq M4_BASE "src/Base.m4")
(setq M4_SRC_REGEXP "USE_MACROS({\\(.*\\)})")
(setq M4_TEMPLATES_SUFFIX "_Macros")
(setq M4_LIST "macro_table.m4")
(setq EXP_SIGNPOST_REGEXP "! \\(\\([A-Z0-9_]*\\) @ \\([0-9]*\\) *\\)")
(setq EXP_BOUNDARY_REGEXP (concat EXP_SIGNPOST_REGEXP "?\\(<<<\\|>>>\\)"))
(setq EXP_DIRECTIVE_REGEXP "EXPAND({\\([A-Z0-9_]*\\).*})")
(setq EXP_TABLE_REGEXP "\\([/A-Za-z0-9_.]*\\) : \\([0-9]*\\) : ")

;; cmake/ctest stuff
(setq CTEST_OPTIONS "-V")

;; environment variable stuff
(setq COMPILER_PREAMBLE
      (concat ". " INTEL_DIR "bin/compilervars.sh ia32 && . " INTEL_DIR "mkl/bin/ia32/mklvars_ia32.sh"))


(shell-command (concat INTEL_DIR "mkl/bin/mklvars.sh ia32"))
(setenv "LD_LIBRARY_PATH"
	(concat INTEL_DIR "lib/ia32:" INTEL_DIR "mkl/lib/ia32"))


(defadvice next-error (before error-in-other-window activate)
  (select-window consoleWindow)
  (select-window (next-window)))


;; === HELPER FUNCTIONS ===

(defun time-stamp-custom ()
  (concat
   (substring (current-time-string) 11 20)    ;; time
   (substring (current-time-string) 0 4)      ;; day (name)
   (substring (current-time-string) 8 11)     ;; day (date)
   (substring (current-time-string) 4 8)      ;; month
   (substring (current-time-string) 20 nil)   ;; year
   ))

;; note to self: the following functions were modified to return nil
;; booleans when a match wasn't made.  Then they were changed back again as
;; the nil return was counter-intuitive.  Keep an eye out for consequent
;; bugs.

;; TODO: look at this!
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html

(defun strip-extension (str)
  "Strips file extensions."
  (with-temp-buffer 
    (insert str) 
    (setq i (search-backward "." nil t))
    (when i
      (push-mark i t) 
      (goto-char (point-max))  
      (delete-region (region-beginning) (region-end)))
    (buffer-string)))

(defun strip-leaf (str)
  "Helper function that turns */foo/bar into */foo/."
  (with-temp-buffer
    (insert str)
    (setq i (search-backward "/")) 
    (push-mark (+ i 1) t) 
    (goto-char (point-max))  
    (delete-region (region-beginning) (region-end))      
    (buffer-string)))

(defun strip-path (str)
  "Helper function that turns */foo/bar into bar."
  (with-temp-buffer 
    (insert str)
    (setq i (search-backward "/")) 
    (push-mark (+ i 1) t) 
    (goto-char (point-min))
    (delete-region (region-beginning) (region-end))
    (buffer-string)))

(defun remove-substring (str substr)
  (with-temp-buffer
    (insert str)
    (when (search-backward substr nil t)
      (push-mark nil t)
      (goto-char (point-min))
      (search-forward substr nil t)
      (delete-region (region-beginning) (region-end)))
    (buffer-string)))


;; the following functions return nil if the resulting string is empty

(defun strip-to-substring (str substr)
  "Strips substr and everything before it in str.  This function has the important property that it returns nil if the substring was not found."
  (let ((result
	 (with-temp-buffer
	   (when str
	     (insert str)
	     (goto-char (point-min))
	     (unless
		 (search-forward substr nil t)
	       (end-of-buffer))
	     (kill-region (point-min) (point)))
	   (buffer-string))))
    (when str
      (when (> (length result) 0)
	result))))

(defun check-dired-mode ()
  (let (diredMode)
    (with-temp-buffer
      (dired-mode)
      (setq diredMode major-mode))
    (equal major-mode diredMode)))


(defun get-local-folder ()
  (let (dir)
    (when (buffer-file-name)      
      ;; if buffer has a file, use its directory.
      (setq dir default-directory))
    (when (check-dired-mode)
      ;; alternatively, buffer could represent a folder in DIRED,
      ;; where the first line contains the path.	
      (goto-char (point-min))
      (search-forward "/" nil t)
      (setq dir (concat (thing-at-point 'symbol) "/")))
    ;; echo output
    dir))

(defun save-unless-temp ()
  (if (buffer-file-name)
      (save-buffer)
    (setq set-buffer-modified-p nil)))

;; ;; exclude these folders from Makefile snippet collection
;; (setq EXCLUDE_FOLDER_LIST '("bin" "mod" "lib" "include"))


;; === SKELETONS ===

(defun makefile-header-skeleton ()
  (concat "# Makefile generated on " (time-stamp-custom)
	  " using " REPOS_DIR "f90-preproc.el

TRUNKDIR = " (substring F90_TRUNK_DIR 0 -1) "
PFUNITDIR = " REPOS_DIR "pFUnit

include $(TRUNKDIR)/" DIRECTIVES_FILENAME "

# PFUNIT = $(wildcard $(PFUNITDIR)/mod/*.o) $(wildcard $(PFUNITDIR)/lib/*.a)
OBJS = $(wildcard *.o)
LIBS = $(wildcard *.a)


.PHONY : all clean tests

all:

tests:	tests.x 

clean:
	rm -rf *.x *.a *.o *.mod *.m4

libpfunit.a:
	cd $(PFUNITDIR) && make all F90_VENDOR=" F90_VENDOR "
	cp $(PFUNITDIR)/source/lib*   .
	cp $(PFUNITDIR)/source/*.mod  .


# The following targets are a mixture of f90 files, m4 expandable files
# and m4 macro definition files.  m4 expandable files produce f90 files.
# These f90 files have chmod rules written in to make them read-only.

# m4 macro definition files are suffixed with " M4_TEMPLATES_SUFFIX ".
# They are not expanded except to form a table of macro names and
# locations, " M4_LIST ", for code navigation purposes.  Unfortunately
# they cannot be compiled, and representing their dependencies is
# difficult because duplicate macro sources cause errors.
# To get round this, m4 files suffixed 'cat' are created.
# They are made from stem m4 files prepended with their dependencies.

# Base_cat.m4 actually need not exist; it is here only to trigger the
# inception of the macro definitions table among other things.
# Originally a phony target dependent on the base m4 was
# created.  But this kept getting invoked for no reason,
# so the real file was created instead.

Base_cat.m4: $(TRUNKDIR)/" M4_BASE "
	cp $(TRUNKDIR)/src/Base.m4 Base_cat.m4
	m4 $(M4_OPT) -P $(TRUNKDIR)/" M4_BASE " > " M4_LIST "
	echo \"m4_divert(-1)\" > pre.m4
	echo \"m4_divert(0)\" > post.m4

"))


(defun template-rule-skeleton (target firstPrerequisite macroPrerequisites)
  (concat target ".m4 : Base_cat.m4 " firstPrerequisite ".m4 " macroPrerequisites "
ifneq \"$(wildcard " target ".m4)\" \"\"
	chmod ugo+w " target ".m4
endif
	cat " macroPrerequisites " " firstPrerequisite ".m4 > " target ".m4
	chmod ugo-w " target ".m4
	m4 $(M4_OPT) -P -I $(TRUNKDIR)/build pre.m4 Base_cat.m4 " macroPrerequisites "post.m4 " firstPrerequisite ".m4 >> " M4_LIST "
" ))


(defun compile-rule-skeleton (target firstPrerequisite otherPrerequisites 
				     isFromMacro macroPrerequisites)
  "Needed because Make isn't able to match the pattern 'bar.o : foo/bar.o'"
  (concat target ".o : " firstPrerequisite
	  (if isFromMacro (concat ".m4 " macroPrerequisites) ".f90 ")
	  otherPrerequisites (when isFromMacro (concat "Base_cat.m4
ifneq \"$(wildcard " firstPrerequisite ".f90)\" \"\"
	chmod ugo+w " firstPrerequisite ".f90
endif
	m4 $(M4_OPT) -P -I $(TRUNKDIR)/build pre.m4 Base_cat.m4 " macroPrerequisites "post.m4 " firstPrerequisite ".m4 > " firstPrerequisite ".f90
	chmod ugo-w " firstPrerequisite ".f90" )) "
	$(F90) -c $(FFLAGS) " firstPrerequisite ".f90
"))

(defun linking-rule-skeleton (target firstPrerequisite otherPrerequisites)
  "Needed because Make isn't able to match the pattern 'bar.o : foo/bar.o'"
  (concat target ": " firstPrerequisite ".f90 " otherPrerequisites "
	rm -rf " F90_TRUNK_DIR "*.x
	$(F90) $(OBJS) " firstPrerequisite
	".f90 -o " target "$(LFLAGS) $(LIBS)"))

(defun driver-skeleton (suiteName)
  "Defines the form of *_driver.f90.  suiteName could be a test
module name or a folder name."
  
  (concat "program " suiteName "_driver

  use pFUnit
  use Echo
  use LogModule
  use " suiteName "_wrap

  implicit none
  type (TestSuite_type) :: suite
  type (TestResult_type) :: result

  ! [problems linking to this method]
  ! call pFUnit_init()

  ! initialise
  call openEchoFile()
  call resetEchoFile()
  call timestamp()
  suite = suite_" suiteName "()
  result = newTestResult(mode=MODE_USE_STDOUT)

  ! run tests and report result
  call Run(suite, result)
  print *, ' '
  print *, trim( Summary(result) )

  ! clean up
  call clean(result)
  call clean(suite)
  call closeEchoFile()

  ! [problems linking to this method]
  ! call pFUnit_finalize

end program " suiteName  "_driver"))


(defun wrapper-skeleton (suiteName useBlock additionsBlock)
  "Defines the form of *_wrap.f90.  suiteName could be a test
module name or a folder name.  additionsBlock is also a
string and takes the form of a series of add(suite, ...) calls."
  (concat "module " suiteName "_wrap

  use pFUnit"	  
	  useBlock "

  implicit none

contains

  function suite_" suiteName "( ) result(suite)
    type (TestSuite_type) :: suite

    suite = TestSuite('" suiteName "')"
    additionsBlock
    "

  end function suite_" suiteName "

end module " suiteName "_wrap"))



;; === FILE AND FOLDER PREPROCESSING ===


(defstruct modStruct name fileBaseName filePath fileUseList 
  macroFileUseList hasSetUp hasTearDown procList hasFixtureList isFromMacro)

(defun scan-file (fileName isTest)
  "Parses fileName and returns information in a mod structure."
  
  ;; start by creating the result structure
  ;; ====
  ;; MODIFICATION (Jan 2012): "and put in the filename base and path"
  ;; - moved this inside the (when) block
  (setq mod (make-modStruct))

  ;; check the file extension.  Must be .f90 to continue parsing
  ;; ====
  ;; MODIFICATION (Jan 2012): must be .f90 wihout an m4 counterpart,
  ;; or .m4 but not Base.m4.
  (when (or (and (string= (substring fileName -3 nil) "f90")
		 (not (file-exists-p
		       (concat (substring fileName 0 -3) "m4"))))
	    (and (string= (substring fileName -2 nil) "m4")
		 (not (string= (strip-path fileName) (strip-path M4_BASE)))))

    (setf (modStruct-isFromMacro mod)
	  (string= (substring fileName -2 nil) "m4"))
    (setf (modStruct-fileBaseName mod)
	  (strip-path (strip-extension fileName)))
    (setf (modStruct-filePath mod)
	  (strip-leaf fileName))
    
    ;; start scanning the file
    (with-temp-buffer
      (insert-file-contents fileName)
      (setq case-fold-search t)

      ;; note to self (29/05/2011)
      ;; use match-string in conjunction with re-search to extract stuff
      ;; rather than trying to manipulate the region
      
      ;; get the name of the module and store as the working name
      (goto-char (point-min))
      (when (re-search-forward "^[ ]*module[ ]*+" nil t)
	(push-mark nil t) (re-search-forward "[ ]*$" nil t)
	(narrow-to-region (region-beginning) (region-end))
	(setf (modStruct-name mod) (thing-at-point 'line))
	(widen))
      
      ;; find the modules used.     ;; Ignore "use pFUnit".
      ;; ignore "ieee_..." (intrinsic)
      ;; and "mkl_..." (math kernel library), etc.
      (setf (modStruct-fileUseList mod) '())
      (while (re-search-forward "^[ ]*use[ ]+" nil t)
	(let ((str (thing-at-point 'symbol)))
	  (unless (or (string=
		       (substring (concat str "     ") 0 5)
		       "ieee_")
		      (string=
		       (substring (concat str "     ") 0 6)
		       "mkl95_")
		      (string=
		       (substring (concat str "     ") 0 8)
		       "lapack95"))
	    ;; (unless (string= str "pFUnit")
	    (setq str (remove-substring str FOLDER_SUFFIX))
	    (setq str (remove-substring str MODULE_SUFFIX))
	    (setf (modStruct-fileUseList mod)
		  (cons str (modStruct-fileUseList mod))))))

      
      ;; search also for the macro source file tag
      (goto-char (point-min))
      (while (re-search-forward M4_SRC_REGEXP nil t)
	  (setf (modStruct-macroFileUseList mod)
		(cons (match-string 1) (modStruct-macroFileUseList mod))))

      (when isTest
	;; look for setUp and tearDown routines
	(goto-char (point-min))
	(setf (modStruct-hasSetUp mod) (re-search-forward "^[ ]*\\(subroutine\\|function\\)[ ]+setup" nil t))
	(goto-char (point-min))
	(setf (modStruct-hasTearDown mod) (re-search-forward "^[ ]*\\(subroutine\\|function\\)[ ]+teardown" nil t))
	
	;; find all procedures prepended with "test"
	(goto-char (point-min))
	(setf (modStruct-procList mod) '())
	(setf (modStruct-hasFixtureList mod) '())
	(while
	    (re-search-forward "^[ ]*\\(pure[ ]+\\|elemental[ ]+\\|recursive[ ]+\\)*\\(subroutine\\|function\\)[ ]+test" nil t)	
	  ;; (backward-char 4) (push-mark nil t) (re-search-forward "[ (^]" nil t)
	  ;; (backward-char 2)
	  ;; (narrow-to-region (region-beginning) (region-end))
	  ;; ;; add procedure name to front of list
	  ;; (setf (modStruct-procList mod)
	  ;;       (cons (buffer-string) (modStruct-procList mod)))
	  ;; (widen)

	  ;; add procedure name to front of list	
	  (setf (modStruct-procList mod)
		(cons (thing-at-point 'symbol) (modStruct-procList mod)))
	  
	  ;; does this procedure come with a fixture (i.e. an argument)?
	  ;; examine the current line for opening parenthesis and string
	  (push-mark nil t) (end-of-line)
	  (narrow-to-region (region-beginning) (region-end))
	  (goto-char (point-min))
	  (setf (modStruct-hasFixtureList mod)
		(cons (re-search-forward "([ 
]*[a-zA-Z0-9_]+" nil t)
		      (modStruct-hasFixtureList mod)))
	  ;; ;; mini-diagnosis
	  ;; (if (car hasFixtureList)	    
	  ;;     (message "t") (message "f"))
	  (widen))

	;; [unnecessary block]
	;; ;; reverse the lists
	;; (setf (modStruct-procList mod)
	;; 	    (reverse (modStruct-procList mod)))
	;; (setf (modStruct-hasFixtureList mod)
	;; 	    (reverse (modStruct-hasFixtureList mod)))
	))
    )   ; end of f90 file parsing
  
  ;; ;; reverse the lists
  ;; (setf (modStruct-fileUseList mod)
  ;; 	(reverse (modStruct-fileUseList mod)))
  
  ;; diagnostics
  (when VERBOSE_MODSCAN
    (message "Module scan diagnostics:")
    (message (concat "  name = " (modStruct-name mod)))
    (message (concat "  file base name = " (modStruct-fileBaseName mod)))
    (message (concat "  file path = " (modStruct-filePath mod)))
    (message "  fileUseList = ")
    (let ((fileUseNode (modStruct-fileUseList mod)))
      (while fileUseNode
	(message (concat "     " (car fileUseNode)))
	(setq fileUseNode (cdr fileUseNode))
	))
    (message "  macroFileUseList = ")
    (let ((macroFileUseNode (modStruct-macroFileUseList mod)))
      (while macroFileUseNode
	(message (concat "     " (car macroFileUseNode)))
	(setq macroFileUseNode (cdr macroFileUseNode))
	))
    
    (when isTest
      (if (modStruct-hasSetUp mod)
	  (message (concat "  setUp() exists."))
	(message (concat "  setUp() does not exist.")))
      (if (modStruct-hasTearDown mod)
	  (message (concat "  tearDown() exists."))
	(message (concat "  tearDown() does not exist.")))
      (message "  procList = ")
      (let ((procNode (modStruct-procList mod))
	    (hasFixtureNode (modStruct-hasFixtureList mod))
	    (hasFixtureMsg nil))
	(while procNode
	  (if (car hasFixtureNode)
	      (setq hasFixtureMsg " [has fixture]")
	    (setq hasFixtureMsg " [no fixture]"))
	  (message (concat "     " (car procNode) hasFixtureMsg))
	  (setq procNode (cdr procNode))
	  (setq hasFixtureNode (cdr hasFixtureNode))
	  ))))
  
  mod)


(defun check-wrap-or-driver (mod)
  "Checks whether mod is wrapper or driver"
  
  (let (flagPass)
    (when (> (length (modStruct-fileBaseName mod)) 5)
      (when (string= (substring (modStruct-fileBaseName mod) -5 nil)
		     "_wrap")
	(setq flagPass t)))
    (when (> (length (modStruct-fileBaseName mod)) 7)
      (when (string= (substring (modStruct-fileBaseName mod) -7 nil)
		     "_driver")
	(setq flagPass t)))
    
    ;; return the flag
    flagPass))


(defun check-test (mod)
  "Checks that mod is a valid test module with test procedures, and is
not a wrapper or driver.  This function can be verbose."

  ;; diagnostics
  (when VERBOSE_MODSCAN (message "Test module check diagnostics:"))
  (let ((iTest 1) (nTest 3) isPass flagFail)
    (while (<= iTest nTest)
      (cond
       ((equal iTest 1) (setq isPass (modStruct-name mod)))       
       ((equal iTest 2) (setq isPass (modStruct-procList mod)))
       ((equal iTest 3) (setq isPass
			      (not (check-wrap-or-driver mod))))
       )
      
      (when VERBOSE_MODSCAN (if isPass (message "  pass") (message "  fail")))
      (setq flagFail (or flagFail (not isPass)))
      (setq isPass nil)
      (setq iTest (1+ iTest)))
    
    (when VERBOSE_MODSCAN
      (if flagFail
	  (message "=> overall fail")
	(message "=> overall pass")))
    
    ;; return a boolean
    (not flagFail)))


(defun format-object (fileBaseName)
  "Returns libpfunit.a if fileBaseName=pFUnit, and fileBaseName.o
otherwise"
  (let (obj)
    (if (string= fileBaseName "pFUnit")
	(setq obj "libpfunit.a ")
      (setq obj (concat fileBaseName ".o ")))))


(defun create-makefile-rule
  (fileBaseName filePath fileUseList isDriver isFromMacro macroFileUseList)
  "Defines a general makefile rule for object compilation."
  (let (target firstPrerequisite otherPrerequisites
	       macroPrerequisites obj (rule ""))
    (when VERBOSE_RULES
      (message (concat "Creating rule for " filePath fileBaseName)))
    (when fileBaseName
      (when (> (length fileBaseName) 0)
	(if isDriver
	    (setq target "tests.x ")
	  (setq target fileBaseName))
	(setq firstPrerequisite (concat
				 "$(TRUNKDIR)/"
				 (strip-to-substring filePath "trunk/")
				 ;; Jan '12: stripped .f90 for flexibility
				 fileBaseName))
	(if (char-or-string-p fileUseList)
	    ;; note that if a single string was passed in fileUseList,
	    ;; this is probably a test driver, so pFUnit is still needed
	    ;; (as well as Echo)
	    (setq otherPrerequisites
		  (concat otherPrerequisites
			  (format-object fileUseList)
			  (format-object "Echo")
			  (format-object "pFUnit")))
	  (progn
	    (while fileUseList
	      (when VERBOSE_RULES
		(message (concat "   with prerequisite "
				 (car fileUseList))))
	      (setq otherPrerequisites
		    (concat otherPrerequisites
			    (format-object (car fileUseList))))
	      (setq fileUseList (cdr fileUseList)))
	    (while macroFileUseList
	      (when VERBOSE_RULES
		(message (concat "   with macro prerequisite "
				 (car macroFileUseList))))
	      (setq macroPrerequisites
		    (concat macroPrerequisites
			    (car macroFileUseList)
			    (concat M4_TEMPLATES_SUFFIX "_cat.m4 ")))
	      (setq macroFileUseList (cdr macroFileUseList)))
	    ))
	(let ((isTemplatesFile))
	  (when (> (length target) (length M4_TEMPLATES_SUFFIX))
	    (setq isTemplatesFile
		  (string= (substring target (- 0 (length M4_TEMPLATES_SUFFIX)) nil)
			   M4_TEMPLATES_SUFFIX)))
	  (if isDriver
	      (setq rule (linking-rule-skeleton
			  target firstPrerequisite otherPrerequisites))
	    (if (and isFromMacro isTemplatesFile)
		(setq rule
		      (template-rule-skeleton
		       (concat target "_cat") firstPrerequisite
		       macroPrerequisites))
	      (setq rule
		    (compile-rule-skeleton
		     target firstPrerequisite otherPrerequisites
		     isFromMacro macroPrerequisites)))
	    ))))
    
    ;; return value
    rule))

(defun write-dependencies-file (dir dependenciesBlock)
  "Writes a dependencies file to dir, unless one already exists and the REGEN_TAG line has been removed.  And when dependenciesBlock is empty.  This is needed by the high-level preprocess functions in addition to the intermediate functions, because a folder wrapper must be generated for the directory level of interest as a companion to the driver program."
  (interactive)
  (let (fileExists flagDoNotWrite)
    (with-temp-buffer
      
      (setq fileExists
	    (file-readable-p (concat dir DEPENDENCIES_FILENAME)))

      ;; check for REGEN_TAG
      (when fileExists
	(insert-file-contents
	 (concat dir DEPENDENCIES_FILENAME))	
	(beginning-of-buffer)
	(unless (search-forward REGEN_TAG nil t)
	  (message (concat "Dependencies have been manually edited in " dir DEPENDENCIES_FILENAME ". Delete that file to enable regeneration."))
	  (setq flagDoNotWrite t))
	(erase-buffer))

      ;; check whether empty.  Delete existing file if appropriate.
      (insert dependenciesBlock)
      (beginning-of-buffer)
      (when VERBOSE_DEPENDENCIES
	(message (concat "Passed to write-dependencies-file: " dependenciesBlock)))
      (unless (re-search-forward "[a-zA-Z0-9_]" nil t)
	(message (concat "Dependencies not detected for " dir))
	(unless flagDoNotWrite
	  (when fileExists
	    (delete-file (concat dir DEPENDENCIES_FILENAME))))	    
	(setq flagDoNotWrite t))
      (erase-buffer)

      ;; start writing dependencies if appropriate
      (unless flagDoNotWrite
	(insert dependenciesBlock)
	
	;; strip empty lines
	(beginning-of-buffer)
	(while (and (re-search-forward "^$" nil t)
		    (not (equal (point) (point-max))))
	  (kill-line))
	
	;; prepend with preamble
	(beginning-of-buffer)
	(insert (concat "# Remove this tag to prevent regeneration: " REGEN_TAG "

# Rules generated on " (time-stamp-custom) "
"))
	(write-region (point-min) (point-max)		      
		      (concat dir
			      DEPENDENCIES_FILENAME) nil)))))


(defun preprocess-file (mod isTest requireDependenciesFile)
  "Unless isTest is set to nil, this takes a mod structure
and from it makes a wrapper module, *_wrap.f90.  The wrapper
provides a single function, suite_*, that compiles all the test
subroutines into a test suite.  A string is also returned
describing the dependencies of the module, regardless of the
state of isTest.  Finally a Makefile fragment is written to the
folder if requireDependenciesFile is non-nil."
  (let ((procNode (modStruct-procList mod))
	(hasFixtureNode (modStruct-hasFixtureList mod)) 
	(fileUseNode (modStruct-fileUseList mod)) flagWarning
	useBlock additionsBlock (dependenciesBlock "") flagNoRegen)
    
    ;; iterate over test procedures creating "call add(suite, ...)"
    ;; strings in additionsBlock
    (when isTest
      (while procNode
	(if (and
	     (modStruct-hasSetUp mod)
	     (modStruct-hasTearDown mod)
	     (or NO_FIXTURE_ARGS (car hasFixtureNode)))
	    ;; procedure with a fixture
	    (setq additionsBlock (concat additionsBlock "
    call add( suite, TestCase1StepFixture(setUp, tearDown, '"
					 (car procNode) "', "
					 (car procNode) ") )"))	
	  (progn	    
	    ;; procedure without a fixture (or, proc with a fixture but
	    ;; setUp/tearDown undefined - this needs a warning flag)
	    (when (and (or (not (modStruct-hasSetUp mod)) (not (modStruct-hasTearDown mod)))
		       (car hasFixtureNode))
	      (setq flagWarning t))
	    (setq additionsBlock (concat additionsBlock "
    call add( suite, TestCase1Step('" (car procNode) "', "
    (car procNode) ") )"))))
	(setq procNode (cdr procNode))
	(setq hasFixtureNode (cdr hasFixtureNode)))
      
      ;; after iterating, check the setUp warning flag
      (when flagWarning (message "WARNING: test procedures with possible fixture arguments exist, but setUp(fixture) and/or tearDown(fixture) is undefined.  Test procedures will be called without the fixture argument."))

      ;; the useBlock is simply "use <module name>"
      (setq useBlock (concat "
  use "
			     (modStruct-name mod)))
      
      ;; now fill in the skeleton and write to file
      (with-temp-buffer
	(insert
	 (wrapper-skeleton (modStruct-name mod) useBlock additionsBlock))
	(write-region (point-min) (point-max)
		      (concat (modStruct-filePath mod) (modStruct-fileBaseName mod) "_wrap.f90") nil)))
    
    ;; ;; describe the dependencies, starting with the .o files
    ;; (while fileUseNode
    ;;   (setq dependenciesBlock
    ;; 	    (concat dependenciesBlock "" (car fileUseNode) ".o "))
    ;;   (setq fileUseNode (cdr fileUseNode)))
    
    ;; ;; prepend this with "*.o : *.f90 "
    ;; (setq dependenciesBlock
    ;; 	  (concat
    ;; 	   ""
    ;; 	   (modStruct-fileBaseName mod) ".o : "
    ;; 	   (strip-to-substring (modStruct-filePath mod) "trunk/")
    ;; 	   (modStruct-fileBaseName mod) ".f90 "
    ;; 	   dependenciesBlock))

    ;; create a rule and add to dependenciesBlock
    (setq dependenciesBlock
	  (concat dependenciesBlock
		  (create-makefile-rule (modStruct-fileBaseName mod)
					(modStruct-filePath mod)
					(modStruct-fileUseList mod)
					nil
					(modStruct-isFromMacro mod)
					(modStruct-macroFileUseList mod)
					)))
    

    ;; note [1]: the block below is involved in a conflict.  If we write
    ;; the wrapper dependencies in this function, preprocess-folder will
    ;; pick them up and further add dependencies from the same wrapper
    ;; code it has detected itself.  So the wrapper dependencies become
    ;; duplicated.  One option is to scrap wrapper dependency generation
    ;; in this function, and always invoke preprocess-folder at the user
    ;; level rather than preprocess-file.  Another option is to keep the
    ;; block here and add conditions to preprocess-folder that allow it
    ;; to skip over wrappers and drivers.  The latter option is
    ;; preferred, since we do not want unwanted drivers being added to
    ;; the makefile indiscriminately.  The same must go for wrappers.

    ;; don't forget dependencies for the wrapper too
    (when isTest
      (setq dependenciesBlock
	    (concat dependenciesBlock "
"
		    (create-makefile-rule
		     (concat (modStruct-fileBaseName mod) "_wrap")
		     (modStruct-filePath mod)
		     (modStruct-fileBaseName mod)
		     nil
		     nil
		     nil))))

    ;; "
    ;; " (modStruct-fileBaseName mod) "_wrap.o : " (strip-to-substring (modStruct-filePath mod) "trunk/") (modStruct-fileBaseName mod) "_wrap.f90 " "" (modStruct-fileBaseName mod) ".o ")))

    (when requireDependenciesFile
      (write-dependencies-file
       (modStruct-filePath mod) dependenciesBlock))

    ;; ;; reset dependenciesBlock if only whitespace
    ;; (with-temp-buffer
    ;;   (insert dependenciesBlock)
    ;;   (beginning-of-buffer)
    ;;   (unless (re-search-forward "[a-zA-Z0-9_]" nil t)
    ;; 	 (setq dependenciesBlock "")))
    
    ;; the output
    dependenciesBlock
    ))


(defun get-folder-file-dependencies (dir isTest)
  "Returns a block string describing the dependencies of modules
at directory level dir.  Contrast this with the dependencies of
folders wrappers at level dir, which is collected by
preprocess-folder.  That function calls
get-folder-file-dependencies once, since it operates on one level
at a time."
  (let (fileList fileNode mod isTestAreYouSure
		 (contentDependenciesBlock ""))
    
    ;; this expression gets a list of present *.f90 files
    ;; ===
    ;; MODIFICATION (Jan 2012): and .m4 files
    (setq fileList
	  (directory-files dir nil "^[a-zA-Z0-9_]*\.\\(f90\\|m4\\)$"))
    
    (when VERBOSE_RECURSION (message "List of local module candidates: "))
    (setq fileNode fileList)
    (while fileNode
      (when VERBOSE_RECURSION (message (concat "  " (car fileNode))))
      (setq fileNode (cdr fileNode)))	  
    
    ;; load each of these, generating wrappers if appropriate and
    ;; if possible.  These wrappers are needed to build the
    ;; higher-level folder wrapper and driver.
    (setq fileNode fileList)
    (while fileNode
      (when VERBOSE_RECURSION
	(message (concat "Inspecting " dir (car fileNode))))
      (setq mod (scan-file (concat dir (car fileNode)) isTest))
      
      ;; continue to generate wrappers and dependencies only if
      ;; the module is itself NOT a wrapper or driver.  Wrapper
      ;; and driver dependencies are generated as a matter of
      ;; course by their respective test modules and folders.  See
      ;; note [1] in preprocess-file.
      (unless (check-wrap-or-driver mod)
	
	;; isTest only describes what we expect to see in this
	;; directory - check whether this is truly a test module,
	;; otherwise a wrapper may be inappropriately generated
	(setq isTestAreYouSure (check-test mod))
	
	;; here, in addition to (possibly) generating wrappers,
	;; the preprocess-file function returns a string of
	;; dependencies which can be appended to
	;; dependenciesBlock.  Set the last arg to nil to ensure
	;; individual Makefile fragments are not created.
	(setq contentDependenciesBlock
	      (concat contentDependenciesBlock (preprocess-file mod isTestAreYouSure nil) "
"))	      
	)
      (setq fileNode (cdr fileNode)))

    ;; return value
    contentDependenciesBlock
    ))



(defun preprocess-folder (dir isTest requireDependenciesFile)
  "A recursive function with two jobs.  (1) If isTest, scans the
given directory looking for processed test modules and folders,
writing to a wrapper in the folder above.  This wrapper's dependencies are
returned in the function output as a string.  (2) Collects makefile
fragments, returning the accumulated fragment in a string.  These
fragments are also written to dependency files in each leaf
folder."
  ;; [not needed]
  ;; ;; create the result structure
  ;; (setq folder (make-folderStruct))
  ;; [initialised folder name here]
  
  ;; This is important - two types of dependencies strings are dealt with
  ;; in this function.  Dependencies of files on the same level as the
  ;; folder are collected in contentDependenciesBlock and may be written
  ;; to a file at that level.  But if the folder generates a wrapper, the
  ;; wrapper's dependencies are returned to the calling (superordinate)
  ;; folder via wrapperDependenciesLine.
  (let (folderName fileList fileNode mod isTreeFolder
		   useBlock additionsBlock isTestAreYouSure
		   (wrapperDependenciesLine "") 
		   (contentDependenciesBlock "")
		   flagNoRegen)
    
    ;; there are two possible situations.  One is that the given path is
    ;; a terminal directory containing test modules.  The other is that
    ;; it is an intermediate directory containing subfolders.  Identify
    ;; whether subfolders exist.    
    (setq fileList
	  (directory-files-and-attributes dir nil "^[a-zA-Z0-9_]*$"))
    ;; after getting a list of files without extensions, examine each
    ;; for the folder attribute    
    (setq fileNode fileList)
    (while fileNode
      ;; the folder attribute is the second node in the attributes
      ;; sublist; it is accessed by e.g. (car (cdr fileNode)).
      (when (car (cdr (car fileNode)))
	;; flag that we are in a tree and then visit this folder
	(setq isTreeFolder t)
	;; (message (car (car fileNode)))
	;; (if (car (cdr (car fileNode))) (message "t") (message "f"))
	(when VERBOSE_RECURSION
	  (message (concat ">>> Entering " dir (car (car fileNode)) "/")))
	;; append the result of this recursive call to the accumulated
	;; result, dependenciesBlock

	;; update - 28/05/2011
	;; the dependencies collected here are those of the WRAPPERS of
	;; subordinate test folders.  The dependencies of files in the
	;; subordinate folders are of no concern at this level.  See notes
	;; under the (let... statement.
	(setq contentDependenciesBlock
	      (concat contentDependenciesBlock "
"
		      (preprocess-folder
		       (concat dir (car (car fileNode)) "/")
		       isTest requireDependenciesFile))))
      
      (setq fileNode (cdr fileNode)))

    ;; after iterating, start examining modules.  When isTest, test
    ;; modules may exist for which wrappers will need to be
    ;; (re)generated in order to build the higher-level wrapper.  All
    ;; modules also need to be scanned for their dependencies.
    
    (when VERBOSE_RECURSION
      (let (folderType)
	(if isTreeFolder (setq folderType "tree") (setq folderType "leaf"))
	(message (concat dir " is a " folderType " folder."))))
    
    ;; join the folder wrapper dependencies with same-level file
    ;; dependencies
    (setq contentDependenciesBlock
	  (concat contentDependenciesBlock "
"
		  (get-folder-file-dependencies dir isTest)))
    
    (when requireDependenciesFile	    
      (when VERBOSE_DEPENDENCIES
	(message (concat "accumulated dependencies for " dir " :
  " contentDependenciesBlock)))
      (write-dependencies-file dir contentDependenciesBlock))  ;; )
    

    ;; If subfolders were found and isTest is t., a wrapper module for
    ;; each will have been generated and placed in the current folder.
    ;; This block will in turn generate a wrapper module and place it
    ;; in the folder above.

    (when isTest
      
      ;; get a list of present *_wrap.f90 files
      (setq fileList
	    (directory-files dir nil "^[a-zA-Z0-9_]*_wrap\.f90$"))
      
      (when VERBOSE_WRAPPING (message "List of local wrappers: "))
      (setq fileNode fileList)
      (while fileNode
	(when VERBOSE_WRAPPING (message (concat "  " (car fileNode))))
	(setq fileNode (cdr fileNode)))
      
      ;; for each of these, extract the module name that also serves as the
      ;; suffix to the suite_* function.  Format and append to the useBlock
      ;; and additionsBlock strings.
      (setq fileNode fileList)
      (let ((fileUseList '("pFUnit")))
	(while fileNode
	  (with-temp-buffer
	    (insert-file-contents (concat dir (car fileNode)))
	    (when (re-search-forward "^[ ]*module[ ]*+" nil t)
	      (push-mark nil t) (re-search-forward "_wrap" nil t)
	      (backward-char 5)
	      (narrow-to-region (region-beginning) (region-end))
	      ;; [not needed]
	      ;; ;; add module name (less _wrap) to front of list
	      ;; (setf (folderStruct-modList folder)
	      ;; 	(cons (thing-at-point 'line)
	      ;; 	      (folderStruct-modList folder)))	  
	      (setq useBlock (concat useBlock "
  use " (buffer-string) "_wrap"))
	      (setq additionsBlock (concat additionsBlock "
    call add( suite, suite_" (buffer-string) "() )"))
	      
	      ;; ;; we will also need to string together the tail end of
	      ;; ;; wrapperDependenciesLine.  
	      ;; (setq wrapperDependenciesLine
	      ;; 	  (concat wrapperDependenciesLine ""
	      ;; 		  (remove-substring
	      ;; 		   (remove-substring (buffer-string) MODULE_SUFFIX)
	      ;; 		   FOLDER_SUFFIX)
	      ;; 		  "_wrap.o "))

	      ;; reconstruct the fileUseList in order to create the rule
	      ;; for this wrapper.  Take care to remove the MODULE_SUFFIX
	      ;; and FOLDER_SUFFIX substrings which may appear in the
	      ;; source code but not the corresponding filenames,
	      ;; and take care to use the original filename, not the source
	      ;; module name.
	      (setq fileUseList
		    (cons
		     (remove-substring
		      (remove-substring
		       (strip-extension (car fileNode))
		       MODULE_SUFFIX)
		      FOLDER_SUFFIX)
		     ;; (cons (buffer-string)
		     fileUseList))
	      (when VERBOSE_WRAPPING
		(message (concat "Interpreted " (buffer-string) " --> "
				 (car fileUseList))))
	      
	      (widen)))	
	  (setq fileNode (cdr fileNode)))
	
	;; write the wrapper to the directory above using
	;; the generic skeleton
	(setq folderName
	      (concat (strip-path (substring dir 0 -1)) FOLDER_SUFFIX))
	(with-temp-buffer
	  (insert
	   (wrapper-skeleton folderName useBlock additionsBlock))
	  (write-region (point-min) (point-max)
			(concat (substring dir 0 -1) "_wrap.f90") nil))

	;;       ;; finally, prepend the dependencies line with '*.o : *.f90 '
	;;       (setq wrapperDependenciesLine 
	;; 	    (concat "
	;; " (strip-path (substring dir 0 -1)) "_wrap.o : " (strip-to-substring (substring dir 0 -1) "trunk/") "_wrap.f90 " wrapperDependenciesLine))

	;; create wrapperDependenciesLine using the compile rule
	(setq wrapperDependenciesLine
	      (create-makefile-rule
	       (concat (strip-path (substring dir 0 -1)) "_wrap")
	       (strip-leaf (substring dir 0 -1))
	       fileUseList
	       nil
	       nil
	       nil))
	)
      
      
      ;; ;; reset wrapperDependenciesLine if only whitespace
      ;; (with-temp-buffer
      ;; 	(insert wrapperDependenciesLine)
      ;; 	(beginning-of-buffer)
      ;; 	(unless (re-search-forward "[a-zA-Z0-9_]" nil t)
      ;; 	  (setq wrapperDependenciesLine "")))
      
      )
    
    
    
    ;; echo the folder's wrapper dependencies
    wrapperDependenciesLine))



;; === PREPROCESSING CALLING FUNCTIONS ===


(defun preprocess-all ()
  "Preprocesses all source and test files.  Writes a final dependencies
file to the trunk level."
  (interactive)
  (save-unless-temp)
  (message " ")
  (message (concat "Preprocessing everything in " F90_TRUNK_DIR))
  (preprocess-folder (concat F90_TRUNK_DIR "src/") nil t)
  (write-dependencies-file
   F90_TRUNK_DIR (preprocess-folder (concat F90_TRUNK_DIR "test/") t t)))


(defun preprocess-local-folder ()
  "Preprocesses the folder associated with the current buffer's file.
Defaults to preprocess-all."
  (interactive)
  (save-unless-temp)
  (let ((dir (get-local-folder)))
    ;; check dir's subordination to the source or test folders
    (if (or (strip-to-substring dir "test/")
	    (strip-to-substring dir "src/"))
	(progn
	  ;; test folders require more work, so it is efficient to check
	  ;; for test folder subordination and inform
	  ;; preprocess-folder
	  (message " ")
	  (message (concat "Preprocessing local folder " dir))
	  (write-dependencies-file
	   (strip-leaf (substring dir 0 -1))
	   (preprocess-folder dir (strip-to-substring dir "test/") t)))
      ;; buffer fails checks.  Process all
      (preprocess-all))))
  

(defun preprocess-local-file ()
  "If the current buffer is a test module, this will do the
preprocessing for that test only.  If it is any other kind of
file in the project directory, the function defaults to
preprocess-local-folder."
  (interactive)
  (save-unless-temp)
  ;; check that the buffer has a file, and that the file is a test
  ;; module
  (if (buffer-file-name)
      (let ((mod (scan-file (buffer-file-name) t)) dependenciesBlock)
	(if (and (check-test mod)
		 (strip-to-substring (get-local-folder) "test/"))
	    ;; single test confirmed; subordination to test/ confirmed.
	    ;; Wrap and write its dependencies.  This may override the
	    ;; (broader) dependency files generated by
	    ;; preprocess-folder.
	    (progn
	      (message " ")
	      (message
	       (concat "Preprocessing local file " (buffer-file-name)))
	      (preprocess-file mod t t))
	  
	  ;; not a test - delegate
	  (preprocess-local-folder)))
    
    ;; not a file but could still be a directory - delegate
    (preprocess-local-folder)))



;; === TEST DRIVER PREPROCESSING ===


(defun driver-rule-preamble ()
  (concat "

# Rule generated on " (time-stamp-custom) "
"))  


(defun create-test-driver (mod)
  "Takes a mod structure and from it makes a driver program, 
*_driver.f90.  Also returns a string describing the dependency of both
the driver and the associated wrapper.  Note that when the driver is
compiled, it becomes the executable invariably named tests.x."
  (let (dependenciesBlock)
    (with-temp-buffer
      (insert
       (driver-skeleton (modStruct-name mod)))
      (write-region (point-min) (point-max)
		    (concat (modStruct-filePath mod)
			    (modStruct-fileBaseName mod)
			    "_driver.f90") nil))        
    (setq dependenciesBlock
	  (concat (driver-rule-preamble)
		  (create-makefile-rule
		   (concat (modStruct-fileBaseName mod) "_driver")
		   (modStruct-filePath mod)
		   (concat (modStruct-fileBaseName mod) "_wrap")
		   t
		   nil
		   nil)))
    ))
				
	  ;; (concat
;; 	   "

;; # Executable dependencies generated on " (time-stamp-custom) "
;; tests.x : " (strip-to-substring (modStruct-filePath mod) "trunk/") (modStruct-fileBaseName mod) "_driver.f90 " "" (modStruct-fileBaseName mod) "_wrap.o
;; ")	  )))


(defun create-test-folder-driver (dir)
  "Takes directory path dir and from it makes a driver program, 
*_driver.f90.  Also returns a string describing the dependency."
  (let* (dependenciesBlock
	 (folderName (concat (strip-path (substring dir 0 -1)) FOLDER_SUFFIX)))
    ;; (setq dir (strip-to-substring dir "trunk/"))
    (with-temp-buffer
      (insert
       (driver-skeleton folderName))
      (write-region
       (point-min) (point-max)
       (concat (substring dir 0 -1) "_driver.f90") nil))    
    (setq dependenciesBlock
	  (concat (driver-rule-preamble)
		  (create-makefile-rule
		   (concat (strip-path (substring dir 0 -1)) "_driver")
		   (strip-leaf (substring dir 0 -1))
		   (concat (strip-path (substring dir 0 -1)) "_wrap")
		   t
		   nil
		   nil)))
    ))

	  
;; 	  (concat
;; 	   "

;; # Executable dependencies generated on " (time-stamp-custom) "
;; tests.x : " (strip-to-substring (substring dir 0 -1) "trunk/") "_driver.f90 " "" (strip-path (substring dir 0 -1)) "_wrap.o
;; "))))
;; mod/" (strip-path (substring dir 0 -1)) "_wrap.o : " (strip-to-substring (substring dir 0 -1) "trunk/") "_wrap.f90" ... ))))



;; === MAKEFILE BUILDING ===


(defun read-dependencies-file (dir requireClean)
  "Looks for and if possible reads the dependencies file in dir.
If requireClean, removes it.  This is to harvest-or-clean-folder
what get-folder-file-dependencies is to preprocess-folder.  It is
also needed by the high-level build-makefile functions, because a
folder wrapper is generated for the directory level of interest
as a companion to the driver program."
  (let (dependenciesBlock)
    (when VERBOSE_DEPENDENCIES
      (message (concat "Looking for " dir DEPENDENCIES_FILENAME)))
    (when (file-readable-p (concat dir DEPENDENCIES_FILENAME))
      (when VERBOSE_DEPENDENCIES (message "  -- found"))
      (if requireClean
	  (progn
	    ;; clean up
	    (delete-file (concat dir DEPENDENCIES_FILENAME))
	    (when VERBOSE_DEPENDENCIES
	      (message (concat "Removed " dir DEPENDENCIES_FILENAME))))
	(progn
	  ;; read from
	  (when VERBOSE_DEPENDENCIES
	    (message (concat "Reading " dir DEPENDENCIES_FILENAME)))
	  (with-temp-buffer
	    (insert-file-contents (concat dir DEPENDENCIES_FILENAME))
	    ;; strip the line containing the REGEN_TAG
	    (beginning-of-buffer)
	    (when (search-forward REGEN_TAG nil t)
	      (kill-line -1))
	    ;; append to accumulated dependenciesBlock
	    (setq dependenciesBlock (buffer-string))))))
    ;; return value
    dependenciesBlock))


(defun harvest-or-clean-folder (dir requireClean)
  "Descends recursively through dir, accumulating the contents of
dependency files and returning in a string.  If requireClean is
non-nil, the function is changed.  Wrappers, drivers and regenerative
dependency files are removed instead."
  (let (folderName fileList fileNode isTreeFolder fileNode2 isSourceFolder
		   useBlock additionsBlock
		   (dependenciesBlock "") flagNoRegen)
    
    ;; some duplicated code is needed to recurse here.  See
    ;; preprocess-folder.
    (setq fileList
	  (directory-files-and-attributes dir nil "^[a-zA-Z0-9_]*$"))
    (setq fileNode fileList)
    ;; iteration over subfolders
    (while fileNode
      (when (car (cdr (car fileNode)))
	(setq isTreeFolder t)

	;; ;; restrict access to source code folders only
	;; (setq fileNode2 SOURCE_FOLDER_LIST)
	;; (setq isSourceFolder nil)
	;; (while (and fileNode2 (not isSourceFolder))
	;;   (message (concat "comparing " 
	;; 		   dir (car (car fileNode)) "/ with "
	;; 		   (car fileNode2)))
	;;   (when (strip-to-substring (concat dir (car (car fileNode)) "/")
	;; 			    (car fileNode2))
	;;     (setq isSourceFolder t))
	;;   (setq fileNode2 (cdr fileNode2)))
	
	;; (when isSourceFolder

	  (when VERBOSE_RECURSION
	    (message (concat ">>> Entering "
			     dir (car (car fileNode)) "/")))
	  ;; append the result of this recursive call to the accumulated
	  ;; result, dependenciesBlock
	  (setq dependenciesBlock
		(concat dependenciesBlock
			(harvest-or-clean-folder
			 (concat dir (car (car fileNode)) "/")
			 requireClean)))) ;; )
      (setq fileNode (cdr fileNode)))

    ;; post-iteration
    (setq dependenciesBlock
	  (concat dependenciesBlock
		  (read-dependencies-file dir requireClean)))
    

    ;; whether a tree or leaf folder, look to cleanup all wrappers and
    ;; drivers
    (when requireClean
      (setq fileList
	    (directory-files
	     dir nil "^[a-zA-Z0-9_]*_\\(wrap\\|driver\\)\.f90$"))
      (setq fileNode fileList)
      (while fileNode
	(delete-file (concat dir (car fileNode)))	
	(message (concat "Removed " dir (car fileNode)))
	(setq fileNode (cdr fileNode))))
    
    dependenciesBlock))


;; (defun list-objects ()
;;   (beginning-of-buffer)
;;   (let ((str "
;; OBJS = "))
;;     (while (and (re-search-forward "^\\([a-zA-Z0-9_]*.o\\)" nil t)
;; 		(not (equal (point) (point-max))))      
;;       (setq str (concat str  " " (match-string 1) " ")))
;;     (setq str (concat str "
;; "))
;;     str))


(defun build-makefile (testBlock)
  "Concatenates the Makefile header, source file dependencies and
test file dependencies into one Makefile.  Source file dependencies are
collected automatically.  Test dependencies vary depending on the level
in the test tree of interest, so they must be passed in the form of
testBlock.  The function return is the executable base name."
  (let (exeBaseName objectString)
    (with-temp-buffer
      (insert
      ;;  (concat (harvest-or-clean-folder (concat F90_TRUNK_DIR "src/") nil)
      ;; 	       testBlock))
      ;; (setq objectString (list-objects))
      ;; (beginning-of-buffer)
      ;; (insert (concat (makefile-header-skeleton) objectString))
      ;; (write-region (point-min) (point-max)
      ;; 		    (concat F90_BUILD_DIR "Makefile") nil)
       (concat (makefile-header-skeleton)
	       (harvest-or-clean-folder (concat F90_TRUNK_DIR "src/") nil)
	       testBlock))
      (write-region (point-min) (point-max)
		    (concat F90_BUILD_DIR "Makefile") nil)
      
      ;; this final list goes to the bottom of the temp buffer and
      ;; extracts the executable base name
      (end-of-buffer)
      (search-backward ".x" nil t)
      (setq exeBaseName (thing-at-point 'symbol)))
    
    ;; echo output
    ;; (when VERBOSE (message (concat "exeBaseName = " exeBaseName)))
    exeBaseName))



;; === MAKEFILE BUILDING CALLING FUNCTIONS ===


(defun build-makefile-for-all ()
  "Writes the Makefile to include all unit tests.  Returns the
base name of the executable (i.e. 'test' in this instance)."
  (interactive)
  (save-unless-temp)
  (message " ")
  (message (concat "Building Makefile for everything in " F90_TRUNK_DIR))
  ;; the executable base name is returned from this call
  (build-makefile
   (concat
    (harvest-or-clean-folder (concat F90_TRUNK_DIR "test/") nil)
    (read-dependencies-file F90_TRUNK_DIR nil)
    (create-test-folder-driver (concat F90_TRUNK_DIR "test/")))))


(defun build-makefile-for-local-folder ()
  "Writes the Makefile for the test folder associated with the
current buffer's file.  If the buffer is in the src tree, a
parallel test folder should exist that can be used.  Defaults to
build-makefile-for-all.  Returns the appropriate folder name"
  (interactive)
  (save-unless-temp)
  (let ((dir (get-local-folder)) subDir basename flagTestDir exeBaseName)
    
    ;; only test folders can be processed in this function.  Check dir's
    ;; subordination to the tests folder
    (setq flagTestDir (strip-to-substring dir "test/"))
    (unless flagTestDir
      (setq subDir (strip-to-substring dir "src/"))
      ;;  If dir instead falls under src, we may try and find a parallel
      ;;  test folder
      (when subDir
	;; first see if this has an equivalent test folder.  Check for a
	;; folder name prepended with "Test", and for the path switch
	;; */src/* to */test/*.	
	(setq basename (concat "Test"
			       (strip-path (strip-extension
					    (buffer-file-name)))))
	(setq dir (concat F90_TRUNK_DIR "test/" subDir basename "/"))
	(message dir)
	(if (file-accessible-directory-p dir)
	    (setq flagTestDir t)
	  ;; go up a folder and try again
	  (progn
	    (setq dir (concat F90_TRUNK_DIR "test/" subDir))
	    (message dir)
	    (when (file-accessible-directory-p dir)
	      (setq flagTestDir t))))))
	
	;; ;; rename dir at this point from */src/* to */test/*
	;; (setq dir (concat F90_TRUNK_DIR "test/" subDir))
	;; (when (file-accessible-directory-p dir)
	;;   (setq flagTestDir t))))
    
    
    (if flagTestDir
	;; successful folder check
	(progn
	  (message " ")
	  (message (concat "Building Makefile for local folder "))
	  (setq exeBaseName
		(build-makefile
		 ;; 2011-08-06: modified to scan the whole folder.
		 ;; This is because source code for test and fake/mock
		 ;; classes existing above the desired test folder or file
		 ;; may have their rules overwritten.
		 (concat (harvest-or-clean-folder
			  (concat F90_TRUNK_DIR "test/") nil)
			 ;; (read-dependencies-file
			 ;;  (strip-leaf (substring dir 0 -1)) nil)
			 (create-test-folder-driver dir)))))
      ;; fail - process all, renaming dir appropriately
      (setq exeBaseName (build-makefile-for-all)))
    
    ;; echo the execuatble base name
    exeBaseName))


(defun build-makefile-for-local-file ()
  "If the current buffer is a test module, this will build a
makefile for that test only.  If it is any other kind of file in
the project directory, the function defaults to
build-makefile-for-local-folder.  Returns the executable name."
  (interactive)
  (save-unless-temp)
  ;; check that the buffer has a file, and that the file is a test
  ;; module
  (let ((dir (buffer-file-name)) exeBaseName flagFile)
    (when dir

      ;; if this is a wrapper or driver, it will still have an
      ;; associated test module, so strip these suffixes from the filename
      (setq dir (remove-substring dir "_wrap"))
      (setq dir (remove-substring dir "_driver"))
      (let ((mod (scan-file dir t)))
	;; now check test module status and subordination to test/.
	(when (and (check-test mod)
		   (strip-to-substring (get-local-folder) "test/"))
	  (setq flagFile t))))
    
    (if flagFile
	;; single test confirmed
	(progn
	  (message " ")
	  (message
	   (concat "Building Makefile for local file "
		   buffer-file-name))
	  (setq exeBaseName
		(build-makefile
		  ;; 2011-08-06: modified to scan the whole folder.
		 ;; This is because source code for test and fake/mock
		 ;; classes existing above the desired test folder or file
		 ;; may have their rules overwritten.
		 (concat
		  (harvest-or-clean-folder
			  (concat F90_TRUNK_DIR "test/") nil)
		  (create-test-driver mod)))))
      ;; not a valid test - delegate
      (setq exeBaseName (build-makefile-for-local-folder)))))
  

;; newer, simpler approach for C++ code
(defun cmake-general ()
  (interactive)
  (shell-command
   (concat "cd " CPP_BUILD_DIR " && cmake ..")))


;; === COMPILATION AND EXECUTION (PREAMBLE) ===

;; preferred window behaviour
(setq-default display-buffer-reuse-frames t)
;; (when (eq nScreens 2)
(add-to-list 'same-window-buffer-names "*compilation*")
(add-to-list 'same-window-buffer-names "*tests*")
(add-to-list 'same-window-buffer-names "*Shell Command Output*")
(add-to-list 'same-window-buffer-names (concat "*gud-" F90_EXE_NAME "*"))
(add-to-list 'same-window-buffer-names (concat "*gud-" CPP_EXE_NAME "*"))
(setq pop-up-frames t)
(setq pop-up-frame-function 'next-frame)
(setq pop-up-windows nil)
(setq split-height-threshold nil)
(setq split-width-threshold nil)
;; )
  

;; --- Error regexps ---
;;
;; this is the f90 compilation error regexp
;; [example]
;;  In file pfft3d.f90:182	     
;;
;;           MPI_double_precision, &
;;                              1
;; Error: Symbol 'mpi_double_precision' at (1) has no IMPLICIT type
;;
;; ALSO regexp(s) for C++
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
	     '(" *In file \\([^ :\n]*\\):\\([0-9]+\\)[ \t]*\n\n.*\n.*\n.*Error:.*\n" 1 2))
(add-to-list 'compilation-error-regexp-alist
	     '("^[0-9]: *\\([A-Za-z0-9\._/]*\\)(\\([0-9]+\\)):" 1 2))
(add-to-list 'compilation-error-regexp-alist
	     '("^CMake Error at \\([A-Za-z0-9\._/]*\\):\\([0-9]+\\)" 1 2))


(defun my-window-configuration-to-register ()
  (interactive)
  (set-register 0 (list (current-window-configuration) nil)))

;; this sets a global variable which is bound to the preferred window for
;; compilation and debugging
(defun set-console-window ()
  (interactive)
  (setq consoleWindow (selected-window))
  ;; remember the current window configuration since gdb will destroy it
  (set-register 0 (list (current-window-configuration) nil))
  (message "Console window and frame layout set"))

(defun reset-console-frame ()
  (interactive)
  (jump-to-register 0))

;; the global variable consoleWindow should have been established in a
;; previous initialisation file.  If it has not, do this now.
(unless (boundp 'consoleWindow)
    (set-console-window))

(defun kill-auto-generated-file-buffers ()
  (when (file-exists-p F90_BUILD_DIR)
    (when (file-exists-p (concat F90_BUILD_DIR "Makefile"))
      (let (fileNode fileList)
	(with-temp-buffer
	  (insert-file-contents (concat F90_BUILD_DIR "Makefile"))
	  (goto-char (point-min))
	  (while (re-search-forward "m4 > $(TRUNKDIR)/.*/\\(.*?.f90\\)" nil t)
	    (setq fileList (cons (match-string 1) fileList)))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\([A-Za-z0-9_]*_cat.m4\\)" nil t)
	    (setq fileList (cons (match-string 1) fileList)))
	  )
	(setq fileNode fileList)
	(while fileNode
	  (when (get-buffer (car fileNode))
	    (message (concat "Killed buffer of auto-generated file "
			     (car fileNode)))
	    (kill-buffer (car fileNode)))
	  (setq fileNode (cdr fileNode)))))))


(defun find-file-if-not-open-already (fileName)
  (if (find-buffer-visiting fileName)
      (switch-to-buffer (find-buffer-visiting fileName))
    (find-file fileName)))


(defun find-file-and-locate-item (fileName lineNum macroName delta)
  (if macroName
      (with-temp-buffer
	(insert-file-contents (concat F90_BUILD_DIR M4_LIST))
	(when VERBOSE_NAVIGATION 
	  (message (concat "Searching for "
			   EXP_TABLE_REGEXP macroName "$")))
	(when (re-search-forward
	       (concat EXP_TABLE_REGEXP macroName "$")
	       nil t)
	  (setq fileName (match-string 1))
	  (setq lineNum (string-to-number (match-string 2)))
	  (when VERBOSE_NAVIGATION 
	    (message (concat "Opening " fileName
			     " at line " (number-to-string lineNum)
			     " plus " (number-to-string delta))))
	  (find-file-if-not-open-already fileName)
	  (goto-char (point-min))
	  (forward-line (+ (- lineNum 1)
			   delta))))
    (when fileName
      (if (file-exists-p fileName)
	  (progn
	    (find-file-if-not-open-already fileName)
	    (when lineNum
	      (goto-char (point-min))
	      (forward-line (+ (- lineNum 1)
			       delta))))
	(message (concat str " does not exist."))))))


(defun pursue-expansion (fileName)
  (let (lineNum (macroName (thing-at-point 'line)) (delta 0))
    (with-temp-buffer
      (insert macroName)
      (goto-char (point-min))
      (when (re-search-forward EXP_DIRECTIVE_REGEXP nil t)
	(setq macroName (match-string 1))))
    (when VERBOSE_NAVIGATION (message (concat "Macro name = " macroName)))
    ;; call the locating function.  Default to jumping back to the
    ;; last f90 file.
    (unless
	(find-file-and-locate-item fileName lineNum macroName delta)
      (when last-f90-buffer
	(switch-to-buffer last-f90-buffer)))))


(defun jump-to-expanding-file (buffer)
  (set-buffer buffer)
  (let ((fileName (buffer-name)) lineNum macroName (delta 0))
    ;; is f90 file => record this
    (setq last-generated-file-buffer (current-buffer))
    ;; get generating file
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "! \\(/[/A-Za-z0-9_.]*\\)" nil t)
	  (setq fileName (match-string 1))
	(setq fileName (concat (substring fileName 0 -4) ".m4"))))
    (when VERBOSE_NAVIGATION 
      (message (concat "Filename = " fileName)))
    ;; find the expansion location
    (let ((i 0))
      (save-excursion
	;; scroll down, keeping track of matching expansion
	;; boundaries
	(while (and (re-search-forward EXP_BOUNDARY_REGEXP nil t)
		    ;; flag if there are one too many closing
		    ;; boundaries - i.e. we were within an expansion.
		    (>= i 0))
	  (if (match-string 1)
	      (setq i (+ i 1))
	    (setq i (- i 1)))
	  ))
      (save-excursion
	;; now scroll up to get the nearest signposted
	;; line number (if there is one)
	(if (re-search-backward EXP_SIGNPOST_REGEXP nil t)
	    (setq lineNum (string-to-number (match-string 3)))
	  (setq lineNum 1)))
      
      (when (>= i 0)
	;; if within an expansion, the target position 
	;; in the m4 file is this signposted number.
	;; otherwise, take the difference between the initial 
	;; position and position of the nearest closing boundary,
	;; and add it to the signposted number.
	(save-excursion
	  ;; search for the position of the nearest closing
	  ;; boundary
	  (unless (re-search-backward EXP_BOUNDARY_REGEXP nil t)
	    ;; we may need to stop at the header (also a signpost)
	    (re-search-backward EXP_SIGNPOST_REGEXP nil t))
	  (setq i (line-number-at-pos)))
	(setq lineNum (+ (- (line-number-at-pos) i)
			 lineNum)))
      )
    (find-file-and-locate-item fileName lineNum macroName delta)))


(defun jump-to-macro-defs-file (buffer)
  (set-buffer buffer)
  (let ((fileName (buffer-name)) lineNum macroName (delta 0))
    
    (let ((i 0) j l)
      (save-excursion
	;; scroll down, keeping track of matching expansion
	;; boundaries.  Keep going to work out the level of nesting.
	(while (re-search-forward EXP_BOUNDARY_REGEXP nil t)
	  (if (match-string 1)
	      (setq i (+ i 1))
	    (setq i (- i 1)))))
      (when VERBOSE_NAVIGATION 
	(message (concat "i = " (number-to-string i))))
      (setq j i)
      (save-excursion
	;; now scroll up to find the right opening expansion
	;; boundary.  Keep track of the distance travelled
	;; at this level, if not the top level.
	(when (< i 0)
	  (setq lineNum (line-number-at-pos))
	  (when VERBOSE_NAVIGATION 
	    (message (concat "start lineNum = "
			     (number-to-string lineNum)))))
	(while (and (re-search-backward EXP_BOUNDARY_REGEXP nil t)
		    (<= j i)
		    (< j 0))
	  ;; get the macro name in the case of an opening expansion
	  (setq macroName (match-string 2))
	  ;; temporarily record this line number
	  (setq l (line-number-at-pos))
	  
	  ;; on the same tier as i and just found a closing
	  ;; boundary?  Update delta
	  (when (and (< i 0) (= j i) (not macroName))
	    (when VERBOSE_NAVIGATION 
	      (message (concat "this line = " (number-to-string l))))
	    (setq delta (+ delta (- lineNum l)))
	    (when VERBOSE_NAVIGATION 
	      (message (concat "new delta = "
			       (number-to-string delta)))))
	  ;; update tier
	  (if macroName
	      (progn
		(setq j (+ j 1)))
	    (progn
	      (setq j (- j 1))))
	  (when VERBOSE_NAVIGATION 
	    (message (concat "new j = " (number-to-string j)))
	    (message (concat "this line = " (number-to-string l))))

	  ;; just re-entered the same tier as i?  Update lineNum
	  (when (and (< i 0) (= j i) macroName)
	    (setq lineNum l)
	    (when VERBOSE_NAVIGATION 
	      (message (concat "This macro name = " macroName))
	      (message (concat "new lineNum = "
			       (number-to-string lineNum)))))
	  )

	;; final update of delta and lineNum.  The latter
	;; will be recomputed later, so nullify it
	(when (< i 0)
	  (when VERBOSE_NAVIGATION 
	    (message (concat "this line = " (number-to-string l))))
	  (setq delta (+ delta (- lineNum l)))
	  (setq lineNum nil)
	  (when VERBOSE_NAVIGATION 
	    (message (concat "final delta = "
			     (number-to-string delta)))))
	(when VERBOSE_NAVIGATION 
	  (message (concat "Macro name = " macroName))
	  (message (concat "j = " (number-to-string j))))
	))
    ;; no macro?  Go to the expanding file instead
    (when (not macroName)
      (when VERBOSE_NAVIGATION 
	(message "No macro here.  Going to expanding file."))
      (find-generator-file))
    (find-file-and-locate-item fileName lineNum macroName delta)))


;; These record the files from which we take our departures.
(setq last-f90-buffer nil)
(setq last-m4-buffer nil)

  
;; will be bound to H-, (<)
(defun find-generator-file ()
  (interactive)
  (let ((fileName (buffer-name)) lineNum macroName (delta 0))
    (cond ((string= (substring fileName -4 nil) ".f90")
	   
	   ;; is f90 file.  Record this before delegating
	   (setq last-f90-buffer (current-buffer))
	   (jump-to-expanding-file last-f90-buffer))
	  
	  ;; next scenario
	  ((string= (substring fileName -3 nil) ".m4")
	   (let (isTemplatesFile)
	     (when (> (length fileName) (+ (length M4_TEMPLATES_SUFFIX) 3))
	       (when (string= (substring
			       fileName
			       (- -3 (length M4_TEMPLATES_SUFFIX))
			       -3)
			      M4_TEMPLATES_SUFFIX)
		 
		 (setq isTemplatesFile t)
		 ;; is M4_TEMPLATES_SUFFIX.m4 file.  We can keep going
		 ;; if we are on an expansion line...
		 (pursue-expansion nil)
		 ))
	     
	     (unless isTemplatesFile
	       ;; is m4 file.  Record this
	       (setq last-m4-buffer (current-buffer))
	       
	       ;; If this is a repeat command, carry
	       ;; the previous buffer over to try and find the 
	       ;; appropriate location in the macro defs file.
	       (if (eq last-command 'find-generator-file)
		   (jump-to-macro-defs-file last-f90-buffer)

		 ;; otherwise, look for M4_TEMPLATES_SUFFIX.m4 file.
		 ;; Are we on an expansion line?  It may override
		 ;; that file.
		 (pursue-expansion (concat (substring fileName 0 -3)
					   M4_TEMPLATES_SUFFIX ".m4"))
	       )))))))

;; will be bound to H-. (>)
(defun find-generated-file ()
  (interactive)
  (let ((fileName (buffer-name)) lineNum macroName (delta 0))
    (cond ((string= (substring fileName -4 nil) ".f90")
	   
	   ;; is f90 file.  Nothing to do.  Try to jump straight to
	   ;; the macro defs file in this instance.
	   (setq last-f90-buffer (current-buffer))
	   (unless (jump-to-macro-defs-file last-f90-buffer)
	     (find-file-if-not-open-already
	      (concat (substring fileName 0 -4)
		      M4_TEMPLATES_SUFFIX ".m4")))
	   )
	   
	  ;; next scenario
	  ((string= (substring fileName -3 nil) ".m4")
	   (let (isTemplatesFile)
	     (when (> (length fileName) (+ (length M4_TEMPLATES_SUFFIX) 3))
	       (when (string= (substring fileName
					 (- -3 (length M4_TEMPLATES_SUFFIX))
					 -3) M4_TEMPLATES_SUFFIX)
		 (setq isTemplatesFile t)
		 ;; is _Templates.m4 file => go back to m4 file,
		 ;; if possible
		 ;; (if last-m4-buffer
		 ;;     (switch-to-buffer last-m4-buffer)
		   (find-file-if-not-open-already
		    (concat (substring
			     fileName 0
			     (- -3 (length M4_TEMPLATES_SUFFIX)))
			    ".m4"))
		   ;; )
		 ))
	     
	     (unless isTemplatesFile
	       ;; is m4 file => go back to f90 file, if possible
	       ;; (if last-f90-buffer
	       ;; 	   (switch-to-buffer last-f90-buffer)
		 (find-file-if-not-open-already
		  (concat (substring fileName 0 -3) ".f90"))
		 ;; )
	       )
	     )))))




;; === COMPILATION AND EXECUTION (MAIN FUNCTIONS) ===

(defun get-trunk-name ()
  (cond ((eq 'f90-mode (buffer-local-value 'major-mode (current-buffer)))
	 F90_TRUNK_NAME)
	(t
	 CPP_TRUNK_NAME)))

(defun get-trunk-dir ()
  (cond ((eq 'f90-mode (buffer-local-value 'major-mode (current-buffer)))
	 F90_TRUNK_DIR)
	(t
	 CPP_TRUNK_DIR)))

(defun get-build-dir ()
  (cond ((eq 'f90-mode (buffer-local-value 'major-mode (current-buffer)))
	 F90_BUILD_DIR)
	(t
	 (let ((str (strip-to-substring (get-local-folder) CPP_TRUNK_NAME)))
	   (if str
	       (setq str (replace-regexp-in-string
			  "^[^/]*/\\(build/\\)?" "" str))
	     (setq str ""))
	   (setq str (concat CPP_BUILD_DIR str))
	   ))
	))

(defun compile-according-to-mode (dir)
  (cond ((eq 'f90-mode (buffer-local-value 'major-mode (current-buffer)))
	 ;; f90 mode
	 (progn
	   (kill-auto-generated-file-buffers)
	   (compile (concat "cd " F90_BUILD_DIR " && " COMPILER_PREAMBLE
			    " && make " F90_EXE_NAME))))
	(t
	 ;; c++/other mode
	 (compile (concat "cd " dir " && make all")))
	))


(defun clean-general ()
  (interactive)
  (setq compilation-read-command nil)  
  (select-window consoleWindow)
  (message " ")
  (compile (concat "cd " (get-build-dir) " && make clean"))
  (setq compilation-read-command t))


(defun test-according-to-mode (&optional with-gui in-directory)
  (cond ((eq 'f90-mode (buffer-local-value 'major-mode (current-buffer)))
	 ;; f90 mode
	 (progn
	   (when (get-buffer (concat "*gud-" F90_EXE_NAME "*"))
	     (set-process-query-on-exit-flag
	      (get-buffer-process (concat "*gud-" F90_EXE_NAME "*")) nil)
	     ;; kill GDB if open
	     (kill-buffer (concat "*gud-" F90_EXE_NAME "*"))
	     (message "Killed existing GDB buffer"))
	   (if with-gui
	       (gdb (concat INTEL_DIR "bin/idb -gdb -fullname "
			    F90_BUILD_DIR F90_EXE_NAME
			    " --command=pre.gdb"))
	     (progn
	       (gdb (concat "gdb --annotate=3"
			    F90_BUILD_DIR F90_EXE_NAME))
	       (insert "run")))))
	(t
	 ;; c++/other mode
	 (progn
	   (when (get-buffer "*tests*")
	     (when (get-buffer-process "*tests*")
	       (set-process-query-on-exit-flag
		(get-buffer-process "*tests*") nil))
	     ;; kill GDB if open
	     (kill-buffer "*tests*"))
	   (let (dir)
	     (if in-directory
		 (setq dir in-directory)
	       (setq dir (get-build-directory)))
	     (shell-command
	      (concat "cd " dir
		      " && ctest " CTEST_OPTIONS)))
	   (with-current-buffer "*Shell Command Output*"
	     (progn
	       (rename-buffer "*tests*")
	       (compilation-mode)
	       (hi-lock-mode)
	       (highlight-regexp "[0-9]+% tests passed.*" 'hi-red-b)
	       (highlight-regexp "100% tests passed.*" 'hi-green-b)
	       (end-of-buffer)
	       ))))
	))

(defun run-general-valgrind (buildDir exeName optionStr)
  "Runs buildDir/exeName under Valgrind."
  (select-window consoleWindow)
  (redisplay)
  (when (get-buffer (concat "*gud-" exeName "*"))
    (kill-buffer (concat "*gud-" exeName "*")) ;; kill GDB if open
    (message "Killed existing GDB buffer"))
  (gdb (concat
	"valgrind " optionStr " " buildDir "" exeName "")))

  

;; === UNIFIED FUNCTIONS ===

;; (defun compile-and-run-local-file ()
;;   "Builds the Makefile, compiles, and runs the current unit test (or
;; failing that an appropriate test directory), but assumes that
;; wrappers and dependencies are up-to-date."
;;   (interactive)
;;   (save-unless-temp)  
;;   (let ((exeBaseName (build-makefile-for-local-file)))
;;     (compile-normal)
;;     (run-normal exeBaseName)))

(defun fully-preprocess-all ()
  "Preprocesses all folders and builds the Makefile."
  (interactive)
  (save-unless-temp)
  (preprocess-all)
  (build-makefile-for-all))

(defun fully-preprocess-local-file ()
  "Preprocesses the current folder and builds the Makefile for the current unit test suite (or, failing that, the nearest test directory)."
  (interactive)
  (save-unless-temp)
  (preprocess-local-folder)
  (build-makefile-for-local-file))


(defun clean-all ()
  "Cleans out all wrappers, drivers and regenerative dependency files
in the trunk directory."
  (interactive)
  (message " ")
  (message (concat "Cleaning everything in " F90_TRUNK_DIR))
  (harvest-or-clean-folder REPOS_DIR t)
  (read-dependencies-file REPOS_DIR t))


(defun clean-local-folder ()
  "Cleans out wrappers, drivers and regenerative dependency file
in the folder associated with the current buffer."
  (interactive)
  (let ((dir (get-local-folder)))   
    ;; check dir's subordination to the trunk folder
    (if (strip-to-substring dir (get-trunk-dir))
	(progn
	  (message " ")
	  (message (concat "Cleaning local folder " dir))
	  (harvest-or-clean-folder
	   (strip-leaf (substring dir 0 -1)) t))
      ;; default to clean-all
      (clean-all))))


(defun get-compile-exit-stat ()
  (switch-to-buffer "*compilation*")
  (string-to-int (substring mode-line-process 7 8)))


(defun compile-and-run-tests (&optional with-gui in-directory)
  "Builds the Makefile, compiles, and runs all tests, but assumes that
wrappers and dependencies are up-to-date."
  (interactive)
  ;; kill Occur
  (when (window-live-p windowOccur)
    (delete-window windowOccur)
    (kill-buffer bufferOccur))
  ;; reset the Occur window/buffer references
  (setq windowOccur nil)
  (setq bufferOccur nil)
  ;; save the current buffer
  (if (buffer-file-name)
      (save-buffer))
  (let (dir)
    (if in-directory
	(setq dir in-directory)
      (setq dir (get-build-dir)))
    (select-window consoleWindow)
    (if (string= mode-line-process ":exit [0]")
	(progn
	  ;; if compilation is complete, run tests
	  (select-window consoleWindow)
	  (redisplay)
	  (test-according-to-mode with-gui dir)
	  )
      (progn
	;; otherwise, compile
	(save-unless-temp)
	(sit-for 0.1)
	(setq compilation-read-command nil)  
	(setq compilation-skip-threshold 2)  
	(select-window consoleWindow)
	(message " ")
	(compile-according-to-mode dir)
	(setq compilation-read-command t))
      )))


(defun run-tests-valgrind ()
  (interactive)
  (select-window consoleWindow)
  (run-general-valgrind (get-build-dir) (get-exe-name) "--leak-check=full --show-reachable=yes --track-origins=yes"))

(defun run-tests-valgrind-interactive (optionStr)
  (interactive "sRun Valgrind with options [--leak-check=full --show-reachable=yes --track-origins=yes]: ")
  (when (string= optionStr "")
    (setq optionStr
	  "--leak-check=full --track-origins=yes"))
  (select-window consoleWindow)
  (run-general-valgrind (get-build-dir) (get-exe-name) optionStr))


(require 'gud)
;; (load-file (concat INTEL_DIR "bin/idb.el"))



;; === QUICK FILE ACCESS ===

(defun find-makefile ()
  "Doubles as CMakeLists.txt finder."
  (interactive)
  (let ((str (concat (strip-leaf (get-local-folder)) "CMakeLists.txt")))
    (if (and (file-exists-p str) (not (eq last-command 'find-makefile)))
	(progn (find-file str)
	       (rename-buffer (strip-to-substring (buffer-file-name)
	       					  (get-trunk-name))))
      (find-file (concat (get-build-dir) "Makefile")))))


(defun find-dependencies-file ()
  (interactive)
  (if (strip-to-substring (concat (get-local-folder) DEPENDENCIES_FILENAME) "trunk/")
      (progn
	(find-file (concat (get-local-folder) DEPENDENCIES_FILENAME))
	(rename-buffer (strip-to-substring (buffer-file-name)
					   (get-trunk-dir))))
    (find-makefile)))

;; (defun switch-src-test ()
;;   "Switches between Foo.f90 and TestFoo.f90 when possible."
;;   (interactive)
;;   (let (

 ;; CTAGS
(setq tag-table-alist '(("" . (concat CPP_TRUNK_DIR "TAGS"))))
(setq tag-table-alist '(("" . (concat F90_TRUNK_DIR "TAGS"))))
(defun create-tags ()
  "Create tags file."
  (interactive)
  (message "Generating tags...")
   (shell-command
    (format "ctags -f %sTAGS -e -R %s" (get-trunk-dir) (get-trunk-dir))))

;; integration with ido
(require 'etags-table)
(defun custom-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))


;; === BINDINGS === 

(global-set-key (kbd "<A-pause>") 'set-console-window)
(global-set-key (kbd "<A-S-pause>") 'reset-console-frame)

(add-hook
 'f90-mode-hook
 (lambda ()
   (define-key f90-mode-map (kbd "A-d")
     'fully-preprocess-local-file)
   (define-key f90-mode-map (kbd "A-D")
     'build-makefile-for-local-file)
   (define-key f90-mode-map (kbd "A-c d")
     'clean-local-folder)
   (define-key f90-mode-map (kbd "A-e")
     'fully-preprocess-all)
   (define-key f90-mode-map (kbd "A-E")
     'build-makefile-for-all)
   (define-key f90-mode-map (kbd "A-e")
     'clean-all)
   (define-key f90-mode-map (kbd "A-,") 'find-generator-file)
   (define-key f90-mode-map (kbd "A-.") 'find-generated-file)
   ))


(define-key global-map (kbd "A-c 7") 'create-tags)
(define-key global-map (kbd "A-7") 'find-tag)
(define-key global-map (kbd "A-8") 'custom-ido-find-tag)
(define-key global-map (kbd "A-&") 'pop-tag-mark)

(global-set-key (kbd "A-d") 'cmake-general)

(global-set-key (kbd "A-<SPC>") 'compile-and-run-tests)
(global-set-key (kbd "A-S-<SPC>") '(lambda () (interactive)
				     (compile-and-run-tests t)))
(global-set-key (kbd "A-x A-<SPC>") 'run-tests-valgrind)
(global-set-key (kbd "A-x <SPC>") 'run-tests-valgrind-interactive)
(global-set-key (kbd "A-c <SPC>") 'clean-general)
(global-set-key (kbd "A-n") 'next-error)
(global-set-key (kbd "A-N") 'previous-error)
(global-set-key (kbd "A-m") 'first-error)

(global-set-key (kbd "<f11>") 'find-makefile)
(global-set-key (kbd "<f12>") 'gdb-many-windows)

(global-set-key (kbd "A-<f11>")
		'(lambda () (interactive)
		   (compile-and-run-tests
		    nil (concat CPP_BUILD_DIR "build_util"))))
