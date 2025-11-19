import cligen
import osproc
import os
import strformat
import strutils
import terminal
import tables
import sequtils

const
  vibVersion = "0.1.0"
  vibAuthor = "michal92299"
  binPath = "/usr/lib/vib/bin"
  plsaBin = binPath & "/PLSA"
  pmBin = binPath & "/package-manager"
  transBin = binPath & "/translator"
  compBin = binPath & "/compiler"
  vmBin = binPath & "/vm"
  currentDate = "November 19, 2025" # Included as per context

proc runExternal(bin: string, args: seq[string]): int =
  let cmd = bin & " " & args.join(" ")
  let (output, exitCode) = execCmdEx(cmd)
  if exitCode != 0:
    styledEcho(fgRed, "Error executing: ", cmd)
    styledEcho(fgRed, output)
  else:
    echo output
  return exitCode

proc checkCode(file: string, deep: bool = true): int =
  var args = @[file]
  if deep:
    args.add("--deep")
  return runExternal(plsaBin, args)

proc install(verbose: bool = false, packages: seq[string]) =
  var args = packages
  if verbose:
    args.add("--verbose")
  discard runExternal(pmBin, @["install"] & args)
  styledEcho(fgGreen, "Installation completed.")

proc remove(force: bool = false, packages: seq[string]) =
  var args = packages
  if force:
    args.add("--force")
  discard runExternal(pmBin, @["remove"] & args)
  styledEcho(fgGreen, "Removal completed.")

proc update(all: bool = false, packages: seq[string] = @[]) =
  var args: seq[string]
  if all:
    args = @["--all"]
  else:
    args = packages
  discard runExternal(pmBin, @["update"] & args)
  styledEcho(fgGreen, "Update completed.")

proc listPackages() =
  # Placeholder list
  styledEcho(fgGreen, "Installed packages:")
  echo "Language  Name     Version"
  echo "vib       mylib    1.0.0"
  echo "python    numpy    1.26.0"

proc cBuild(target = "", output = "", optimize: bool = false, file: string) =
  if checkCode(file) != 0:
    quit(1)
  var args = @["build", file]
  if target != "":
    args.add(["--target", target])
  if output != "":
    args.add(["--output", output])
  if optimize:
    args.add("--optimize")
  discard runExternal(compBin, args)
  styledEcho(fgGreen, "Build completed.")

proc cClean(all: bool = false) =
  var args = @["clean"]
  if all:
    args.add("--all")
  discard runExternal(compBin, args)
  styledEcho(fgGreen, "Clean completed.")

proc cTest(file: string) =
  styledEcho(fgYellow, fmt"Running tests on {file} (placeholder)")

proc vBuild(release: bool = false, output = "", file: string) =
  if checkCode(file) != 0:
    quit(1)
  var args = @["build", file]
  if release:
    args.add("--release")
  if output != "":
    args.add(["--output", output])
  discard runExternal(vmBin, args)
  styledEcho(fgGreen, "VM build completed.")

proc vCompile(target = "", file: string) =
  if checkCode(file, false) != 0:
    quit(1)
  var args = @["compile", file]
  if target != "":
    args.add(["--target", target])
  discard runExternal(vmBin, args)
  styledEcho(fgGreen, "VM compile completed.")

proc vClean() =
  discard runExternal(vmBin, @["clean"])
  styledEcho(fgGreen, "VM clean completed.")

proc vRun(file: string) =
  discard runExternal(vmBin, @["run", file])
  styledEcho(fgGreen, "VM run completed.")

proc tCompile(target = "", output = "", lang: string, file: string) =
  if checkCode(file) != 0:
    quit(1)
  var args = @[file]
  if target != "":
    args.add(["--target", target])
  if output != "":
    args.add(["--output", output])
  if lang.toLowerAscii == "c":
    discard runExternal(transBin, @[lang] & args)
  else:
    # Docker
    let dir = parentDir(file)
    let fname = extractFilename(file)
    var dockerArgs = @["run", "--rm", "-v", fmt"{dir}:/app", "-w", "/app", fmt"vib-translator-{lang}", "translate", fname]
    dockerArgs.add(args[1..^1])
    discard runExternal("docker", dockerArgs)
  styledEcho(fgGreen, "Translation and compile completed.")

proc tList() =
  let supported = @["c", "zig", "rust", "python", "java", "go"]
  styledEcho(fgGreen, "Supported languages:")
  for l in supported.sorted:
    echo "- " & l

proc run(debug: bool = false, file: string) =
  if checkCode(file) != 0:
    quit(1)
  var args = @[file]
  if debug:
    args.add("--debug")
  discard runExternal(vmBin, args)
  styledEcho(fgGreen, "Run completed.")

proc initProject(license = "MIT", repo = "", projectName = "my-vib-project") =
  createDir(projectName)
  let cmdDir = projectName & "/cmd"
  createDir(cmdDir)
  let mainFile = cmdDir & "/main.vib"
  let exampleCode = """function main() [
  write("Hello from Vib!")
]"""
  writeFile(mainFile, exampleCode)
  let vmlFile = projectName & "/Project.vml"
  # Simple TOML string, assuming no lib, or use parsetoml if installed
  let vmlContent = fmt"""name = "{projectName}"
license = "{license}"
repository = "{repo}"
version = "0.1.0"
[dependencies]
build_targets = ["exe", "bin", "deb", "rpm"]
source_dir = "cmd/main.vib"
"""
  writeFile(vmlFile, vmlContent)
  styledEcho(fgGreen, fmt"Initialized project {projectName} with license {license} and repo {repo}")

proc tutorials(topic = "") =
  if topic == "":
    echo "Available topics: syntax, functions, embedded, libraries, comments, date"
    return
  case topic.toLowerAscii
  of "syntax":
    echo "Vib syntax: Python-like with [] blocks, JS functions."
  of "functions":
    echo "function name(params) [ body ]"
  of "embedded":
    echo "#=lang= [ code in lang ]"
  of "libraries":
    echo "require \"lang:name\" ; libraries in <> like <python:numpy>"
  of "comments":
    echo "~ single :multi::"
  of "date":
    echo "Current date: " & currentDate
  else:
    styledEcho(fgRed, fmt"Unknown topic: {topic}")

proc doc(file: string) =
  styledEcho(fgYellow, fmt"Generating docs for {file} (placeholder)")

proc showAuthor() =
  styledEcho(fgBlue, "Author: " & vibAuthor)

dispatchMulti(
  [install, help = {"verbose": "Enable verbose output", "packages": "Packages to install"}],
  [remove, help = {"force": "Force removal", "packages": "Packages to remove"}],
  [update, help = {"all": "Update all", "packages": "Packages to update"}],
  [listPackages, cmdName = "list"],
  [cBuild, cmdName = "c build", help = {"target": "Cross-compile target", "output": "Output file", "optimize": "Optimize", "file": ".vib file"}],
  [cClean, cmdName = "c clean", help = {"all": "Clean all"}],
  [cTest, cmdName = "c test", help = {"file": "File to test"}],
  [vBuild, cmdName = "v build", help = {"release": "Release mode", "output": "Output file", "file": ".vib file"}],
  [vCompile, cmdName = "v compile", help = {"target": "Target", "file": ".vib-vm file"}],
  [vClean, cmdName = "v clean"],
  [vRun, cmdName = "v run", help = {"file": ".object file"}],
  [tCompile, cmdName = "t compile", help = {"target": "Target", "output": "Output", "lang": "Language", "file": ".vib file"}],
  [tList, cmdName = "t list"],
  [run, help = {"debug": "Debug mode", "file": ".vib file"}],
  [initProject, cmdName = "init", help = {"license": "License", "repo": "Repo URL", "projectName": "Project name"}],
  [tutorials, help = {"topic": "Tutorial topic"}],
  [doc, help = {"file": ".vib file"}],
  [showAuthor, cmdName = "autor"]
)
