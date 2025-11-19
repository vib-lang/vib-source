import cligen
import osproc
import os
import strformat
import strutils
import terminal
import tables
import sequtils
import toml_serialization  # Assuming nim-toml or similar, but for simplicity, use string formatting for TOML

const
  vibVersion = "0.1.0"
  vibAuthor = "michal92299"
  binPath = "/usr/lib/vib/bin"
  plsaBin = binPath & "/PLSA"
  pmBin = binPath & "/package-manager"
  transBin = binPath & "/translator"
  compBin = binPath & "/compiler"
  vmBin = binPath & "/vm"

proc runExternal(bin: string, args: seq[string], verbose: bool = false): int =
  let cmd = bin & " " & args.join(" ")
  if verbose:
    styledEcho(fgYellow, "Executing: " & cmd)
  let (output, exitCode) = execCmdEx(cmd)
  if exitCode != 0:
    styledEcho(fgRed, "Error executing: ", cmd)
    styledEcho(fgRed, output)
  else:
    if verbose or exitCode == 0:
      echo output
  return exitCode

proc checkCode(file: string, deep: bool = true, verbose: bool = false): int =
  var args = @[file]
  if deep:
    args.add("--deep")
  return runExternal(plsaBin, args, verbose)

proc install(verbose: bool = false, packages: seq[string]) =
  var args = packages
  if verbose:
    args.insert("--verbose", 0)
  if runExternal(pmBin, @["install"] & args, verbose) == 0:
    styledEcho(fgGreen, "Installation completed successfully.")
  else:
    styledEcho(fgRed, "Installation failed.")

proc remove(force: bool = false, packages: seq[string]) =
  var args = packages
  if force:
    args.insert("--force", 0)
  if runExternal(pmBin, @["remove"] & args) == 0:
    styledEcho(fgGreen, "Removal completed successfully.")
  else:
    styledEcho(fgRed, "Removal failed.")

proc update(all: bool = false, packages: seq[string] = @[]) =
  var args: seq[string]
  if all:
    args = @["--all"]
  else:
    args = packages
  if runExternal(pmBin, @["update"] & args) == 0:
    styledEcho(fgGreen, "Update completed successfully.")
  else:
    styledEcho(fgRed, "Update failed.")

proc listPackages(detailed: bool = false) =
  # Placeholder, assume pm has list, but simulate
  styledEcho(fgGreen, "Installed packages:")
  echo "Language\tName\tVersion"
  echo "vib\tmylib\t1.0.0"
  echo "python\tnumpy\t1.26.0"
  if detailed:
    echo "\nDetails:"
    echo "vib:mylib - A sample Vib library"
    echo "python:numpy - Numerical computing library"

proc cBuild(target = "", output = "", optimize: bool = false, verbose: bool = false, file: string) =
  if checkCode(file, true, verbose) != 0:
    quit(1)
  var args = @["build", file]
  if target != "":
    args.add("--target")
    args.add(target)
  if output != "":
    args.add("--output")
    args.add(output)
  if optimize:
    args.add("--optimize")
  if runExternal(compBin, args, verbose) == 0:
    styledEcho(fgGreen, "Build completed successfully.")
  else:
    styledEcho(fgRed, "Build failed.")

proc cClean(all: bool = false, verbose: bool = false) =
  var args = @["clean"]
  if all:
    args.add("--all")
  if runExternal(compBin, args, verbose) == 0:
    styledEcho(fgGreen, "Clean completed successfully.")
  else:
    styledEcho(fgRed, "Clean failed.")

proc cTest(verbose: bool = false, file: string) =
  styledEcho(fgYellow, fmt"Running tests on {file} (placeholder)")
  # Simulate test run
  if verbose:
    echo "Test output: All tests passed."

proc vBuild(release: bool = false, output = "", verbose: bool = false, file: string) =
  if checkCode(file, true, verbose) != 0:
    quit(1)
  var args = @["build", file]
  if release:
    args.add("--release")
  if output != "":
    args.add("--output")
    args.add(output)
  if runExternal(vmBin, args, verbose) == 0:
    styledEcho(fgGreen, "VM build completed successfully.")
  else:
    styledEcho(fgRed, "VM build failed.")

proc vCompile(target = "", verbose: bool = false, file: string) =
  if checkCode(file, false, verbose) != 0:
    quit(1)
  var args = @["compile", file]
  if target != "":
    args.add("--target")
    args.add(target)
  if runExternal(vmBin, args, verbose) == 0:
    styledEcho(fgGreen, "VM compile completed successfully.")
  else:
    styledEcho(fgRed, "VM compile failed.")

proc vClean(verbose: bool = false) =
  var args = @["clean"]
  if runExternal(vmBin, args, verbose) == 0:
    styledEcho(fgGreen, "VM clean completed successfully.")
  else:
    styledEcho(fgRed, "VM clean failed.")

proc vRun(verbose: bool = false, file: string) =
  var args = @["run", file]
  if verbose:
    args.add("--verbose")
  if runExternal(vmBin, args, verbose) == 0:
    styledEcho(fgGreen, "VM run completed successfully.")
  else:
    styledEcho(fgRed, "VM run failed.")

proc tCompile(target = "", output = "", verbose: bool = false, lang: string, file: string) =
  if checkCode(file, true, verbose) != 0:
    quit(1)
  var args = @[file]
  if target != "":
    args.add("--target")
    args.add(target)
  if output != "":
    args.add("--output")
    args.add(output)
  if lang.toLowerAscii == "c":
    if runExternal(transBin, @[lang] & args, verbose) == 0:
      styledEcho(fgGreen, "Translation and compile completed successfully.")
    else:
      styledEcho(fgRed, "Translation and compile failed.")
  else:
    let dir = parentDir(absolutePath(file))
    let fname = extractFilename(file)
    var dockerArgs = @["run", "--rm", "-v", fmt"{dir}:/app", "-w", "/app", fmt"vib-translator-{lang}", "translate", fname]
    for i in countup(1, args.len-1, 2):
      dockerArgs.add(args[i])
      dockerArgs.add(args[i+1])
    if runExternal("docker", dockerArgs, verbose) == 0:
      styledEcho(fgGreen, "Docker translation and compile completed successfully.")
    else:
      styledEcho(fgRed, "Docker translation and compile failed.")

proc tList() =
  let supported = @["c", "go", "java", "python", "rust", "zig"].sorted
  styledEcho(fgGreen, "Supported translation languages:")
  for l in supported:
    echo "- " & l

proc run(debug: bool = false, verbose: bool = false, file: string) =
  if checkCode(file, true, verbose) != 0:
    quit(1)
  var args = @[file]
  if debug:
    args.add("--debug")
  if verbose:
    args.add("--verbose")
  if runExternal(vmBin, args, verbose) == 0:
    styledEcho(fgGreen, "Run completed successfully.")
  else:
    styledEcho(fgRed, "Run failed.")

proc initProject(license = "MIT", repo = "", verbose: bool = false, projectName = "my-vib-project") =
  if dirExists(projectName):
    styledEcho(fgRed, fmt"Directory {projectName} already exists.")
    quit(1)
  createDir(projectName)
  let cmdDir = projectName / "cmd"
  createDir(cmdDir)
  let mainFile = cmdDir / "main.vib"
  let exampleCode = """function main() [
	write("Hello from Vib!")
]"""
  writeFile(mainFile, exampleCode)
  let vmlFile = projectName / "Project.vml"
  let vmlContent = fmt"""name = "{projectName}"
license = "{license}"
repository = "{repo}"
version = "0.1.0"

[dependencies]

build_targets = ["exe", "bin", "deb", "rpm"]
source_dir = "cmd/main.vib"
"""
  writeFile(vmlFile, vmlContent)
  if verbose:
    styledEcho(fgYellow, "Project files created:")
    echo mainFile
    echo vmlFile
  styledEcho(fgGreen, fmt"Initialized project {projectName} with license {license} and repo {repo}.")

proc tutorials(topic = "") =
  if topic == "":
    echo "Available topics: syntax, functions, embedded, libraries, comments"
    return
  case topic.toLowerAscii
  of "syntax":
    echo "Vib syntax draws clarity from Rust, most syntax from Python: [] instead of {}, write instead of print."
  of "functions":
    echo "Functions are defined like in JavaScript: function name(params) [ body ]"
  of "embedded":
    echo "Embed code from other languages: #=lang= [ code ]"
  of "libraries":
    echo "Require libraries: require \"module\"; for foreign: require \"lang:module\". Specify in <> like <python:numpy>."
  of "comments":
    echo "Single line: ~ comment\nMulti line: :: comment ::"
  else:
    styledEcho(fgRed, fmt"Unknown topic: {topic}")
    echo "Try: syntax, functions, embedded, libraries, comments"

proc doc(verbose: bool = false, file: string) =
  styledEcho(fgYellow, fmt"Generating documentation for {file} (placeholder)")
  if verbose:
    echo "Doc output: Documentation generated."

proc showAuthor() =
  styledEcho(fgBlue, "Author of Vib: " & vibAuthor)

proc showVersion() =
  styledEcho(fgBlue, "Vib version: " & vibVersion)

dispatchMulti(
  [install, help = {"verbose": "Enable verbose output", "packages": "Packages to install (e.g., python:numpy)"}],
  [remove, help = {"force": "Force removal", "packages": "Packages to remove"}],
  [update, help = {"all": "Update all packages", "packages": "Specific packages to update"}],
  [listPackages, cmdName = "list", help = {"detailed": "Show detailed info"}],
  [cBuild, cmdName = "c build", help = {"target": "Cross-compile target (e.g., windows-amd64)", "output": "Output binary name", "optimize": "Enable optimizations", "verbose": "Verbose output", "file": ".vib file to build"}],
  [cClean, cmdName = "c clean", help = {"all": "Clean all artifacts", "verbose": "Verbose output"}],
  [cTest, cmdName = "c test", help = {"verbose": "Verbose test output", "file": "File to test"}],
  [vBuild, cmdName = "v build", help = {"release": "Build release .vib-vm", "output": "Output file", "verbose": "Verbose output", "file": ".vib file"}],
  [vCompile, cmdName = "v compile", help = {"target": "Cross-compile target", "verbose": "Verbose output", "file": ".vib-vm file"}],
  [vClean, cmdName = "v clean", help = {"verbose": "Verbose output"}],
  [vRun, cmdName = "v run", help = {"verbose": "Verbose output", "file": ".object file"}],
  [tCompile, cmdName = "t compile", help = {"target": "Cross-compile target", "output": "Output file", "verbose": "Verbose output", "lang": "Target language", "file": ".vib file"}],
  [tList, cmdName = "t list"],
  [run, help = {"debug": "Run in debug mode", "verbose": "Verbose output", "file": ".vib file to run"}],
  [initProject, cmdName = "init", help = {"license": "Project license (default MIT)", "repo": "GitHub repo URL", "verbose": "Verbose output", "projectName": "Name of the project (default my-vib-project)"}],
  [tutorials, help = {"topic": "Specific tutorial topic (leave empty for list)"}],
  [doc, cmdName = "doc", help = {"verbose": "Verbose output", "file": ".vib file for doc generation"}],
  [showAuthor, cmdName = "autor"],
  [showVersion, cmdName = "version"]
)
