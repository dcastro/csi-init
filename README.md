#csi-init
`csi-init` is a command line tool that allows you to launch [Roslyn's C# REPL (`csi`)][0] preloaded with all the assemblies found in one or more directories.

## Usage

You can use `-d` to load the REPL with all the assemblies found in a given directory, or `-r` to search for assemblies recursively.

E.g. `csi-init -d c:\MyProject\bin`

All arguments specified after `--` will be passed directly to `csi`.

E.g. `csi-init -d c:\MyProject\bin -- /u:MyProject.SomeNamespace`

A common use case is to download all your commonly used NuGet packages into a folder (using [nuget's command line tool][3]), and then load all these assemblies into a REPL:

```
$ mkdir c:\CommonPackages
$ cd c:\CommonPackages
$ nuget install Newtonsoft.Json
$ nuget install LanguageExt.Core
$ nuget install xunit
$ csi-init -r c:\CommonPackages
```

## Download

See https://github.com/dcastro/csi-init/releases

## Building

Using [stack][1]:

```
$ stack build
$ stack exec csi-init
```

Or:

```
$ stack build && stack install
$ csi-init
```

## Available Options

`csi-init`'s options:
```
$ csi -h
Invokes the `csi` C# REPL preloaded with a bunch of assemblies.

Usage: csi-init [-r|--rdir ASSEMBLYDIR] [-d|--dir ASSEMBLYDIR] [ARGUMENTS...]
                [--debug]
  All arguments specified after `--` will be passed to `csi`. E.g.: `csi-init --
  /u:Newtonsoft.Json`

Available options:
  -h,--help                Show this help text
  -r,--rdir ASSEMBLYDIR    Import all assemblies from the specified folder and
                           it's subfolders (recursive search)
  -d,--dir ASSEMBLYDIR     Import all assemblies from the specified folder
  --debug                  Print the arguments being passed to `csi`
```

`csi`'s options (at the moment of writing):
```
$ csi -help
Microsoft (R) Visual C# Interactive Compiler version 1.3.1.60616
Copyright (C) Microsoft Corporation. All rights reserved.

Usage: csi [option] ... [script-file.csx] [script-argument] ...

Executes script-file.csx if specified, otherwise launches an interactive REPL (Read Eval Print Loop).

Options:
  /help                          Display this usage message (alternative form: /?)
  /i                             Drop to REPL after executing the specified script.
  /r:<file>                      Reference metadata from the specified assembly file (alternative form: /reference)
  /r:<file list>                 Reference metadata from the specified assembly files (alternative form: /reference)
  /lib:<path list>               List of directories where to look for libraries specified by #r directive.
                                 (alternative forms: /libPath /libPaths)
  /u:<namespace>                 Define global namespace using (alternative forms: /using, /usings, /import, /imports)
  @<file>                        Read response file for more options
  --                             Indicates that the remaining arguments should not be treated as options.
```



 [0]: https://github.com/dotnet/roslyn/wiki/Interactive-Window#repl
 [1]: https://haskell-lang.org/get-started
 [3]: https://dist.nuget.org/index.html
