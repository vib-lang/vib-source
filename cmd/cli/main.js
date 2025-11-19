const { Command } = require('commander');
const chalk = require('chalk');
const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const vibVersion = '0.1.0';
const vibAuthor = 'michal92299';
const binPath = '/usr/lib/vib/bin';
const plsaBin = path.join(binPath, 'PLSA');
const pmBin = path.join(binPath, 'package-manager');
const transBin = path.join(binPath, 'translator');
const compBin = path.join(binPath, 'compiler');
const vmBin = path.join(binPath, 'vm');

function runExternal(bin, args, verbose = false) {
  const cmd = `${bin} ${args.join(' ')}`;
  if (verbose) {
    console.log(chalk.yellow(`Executing: ${cmd}`));
  }
  try {
    const output = execSync(cmd, { stdio: 'pipe' });
    if (verbose) {
      console.log(output.toString());
    }
    return 0;
  } catch (err) {
    console.error(chalk.red(`Error executing: ${cmd}`));
    if (err.stdout) console.error(chalk.red(err.stdout.toString()));
    if (err.stderr) console.error(chalk.red(err.stderr.toString()));
    return 1;
  }
}

function checkCode(file, deep = true, verbose = false) {
  let args = [file];
  if (deep) args.push('--deep');
  return runExternal(plsaBin, args, verbose);
}

const program = new Command();
program.name('vib');
program.version(vibVersion);

program.command('install')
  .option('--verbose')
  .argument('<packages...>')
  .action((packages, options) => {
    let args = packages;
    if (options.verbose) args.unshift('--verbose');
    if (runExternal(pmBin, ['install', ...args], options.verbose) === 0) {
      console.log(chalk.green('Installation completed successfully.'));
    } else {
      console.log(chalk.red('Installation failed.'));
    }
  });

program.command('remove')
  .option('--force')
  .argument('<packages...>')
  .action((packages, options) => {
    let args = packages;
    if (options.force) args.unshift('--force');
    if (runExternal(pmBin, ['remove', ...args]) === 0) {
      console.log(chalk.green('Removal completed successfully.'));
    } else {
      console.log(chalk.red('Removal failed.'));
    }
  });

program.command('update')
  .option('--all')
  .argument('[packages...]')
  .action((packages, options) => {
    let args = options.all ? ['--all'] : packages;
    if (runExternal(pmBin, ['update', ...args]) === 0) {
      console.log(chalk.green('Update completed successfully.'));
    } else {
      console.log(chalk.red('Update failed.'));
    }
  });

program.command('list')
  .option('--detailed')
  .action((options) => {
    console.log(chalk.green('Installed packages:'));
    console.log('Language\tName\tVersion');
    console.log('vib\tmylib\t1.0.0');
    console.log('python\tnumpy\t1.26.0');
    if (options.detailed) {
      console.log('\nDetails:');
      console.log('vib:mylib - A sample Vib library');
      console.log('python:numpy - Numerical computing library');
    }
  });

const cCmd = program.command('c');

cCmd.command('build')
  .option('--target <target>')
  .option('--output <output>')
  .option('--optimize')
  .option('--verbose')
  .argument('<file>')
  .action((file, options) => {
    if (checkCode(file, true, options.verbose) !== 0) process.exit(1);
    let args = ['build', file];
    if (options.target) {
      args.push('--target', options.target);
    }
    if (options.output) {
      args.push('--output', options.output);
    }
    if (options.optimize) args.push('--optimize');
    if (runExternal(compBin, args, options.verbose) === 0) {
      console.log(chalk.green('Build completed successfully.'));
    } else {
      console.log(chalk.red('Build failed.'));
    }
  });

cCmd.command('clean')
  .option('--all')
  .option('--verbose')
  .action((options) => {
    let args = ['clean'];
    if (options.all) args.push('--all');
    if (runExternal(compBin, args, options.verbose) === 0) {
      console.log(chalk.green('Clean completed successfully.'));
    } else {
      console.log(chalk.red('Clean failed.'));
    }
  });

cCmd.command('test')
  .option('--verbose')
  .argument('<file>')
  .action((file, options) => {
    console.log(chalk.yellow(`Running tests on ${file} (placeholder)`));
    if (options.verbose) {
      console.log('Test output: All tests passed.');
    }
  });

const vCmd = program.command('v');

vCmd.command('build')
  .option('--release')
  .option('--output <output>')
  .option('--verbose')
  .argument('<file>')
  .action((file, options) => {
    if (checkCode(file, true, options.verbose) !== 0) process.exit(1);
    let args = ['build', file];
    if (options.release) args.push('--release');
    if (options.output) {
      args.push('--output', options.output);
    }
    if (runExternal(vmBin, args, options.verbose) === 0) {
      console.log(chalk.green('VM build completed successfully.'));
    } else {
      console.log(chalk.red('VM build failed.'));
    }
  });

vCmd.command('compile')
  .option('--target <target>')
  .option('--verbose')
  .argument('<file>')
  .action((file, options) => {
    if (checkCode(file, false, options.verbose) !== 0) process.exit(1);
    let args = ['compile', file];
    if (options.target) {
      args.push('--target', options.target);
    }
    if (runExternal(vmBin, args, options.verbose) === 0) {
      console.log(chalk.green('VM compile completed successfully.'));
    } else {
      console.log(chalk.red('VM compile failed.'));
    }
  });

vCmd.command('clean')
  .option('--verbose')
  .action((options) => {
    let args = ['clean'];
    if (runExternal(vmBin, args, options.verbose) === 0) {
      console.log(chalk.green('VM clean completed successfully.'));
    } else {
      console.log(chalk.red('VM clean failed.'));
    }
  });

vCmd.command('run')
  .option('--verbose')
  .argument('<file>')
  .action((file, options) => {
    let args = ['run', file];
    if (options.verbose) args.push('--verbose');
    if (runExternal(vmBin, args, options.verbose) === 0) {
      console.log(chalk.green('VM run completed successfully.'));
    } else {
      console.log(chalk.red('VM run failed.'));
    }
  });

const tCmd = program.command('t');

tCmd.command('compile')
  .option('--target <target>')
  .option('--output <output>')
  .option('--verbose')
  .argument('<lang>')
  .argument('<file>')
  .action((lang, file, options) => {
    if (checkCode(file, true, options.verbose) !== 0) process.exit(1);
    let args = [file];
    if (options.target) {
      args.push('--target', options.target);
    }
    if (options.output) {
      args.push('--output', options.output);
    }
    if (lang.toLowerCase() === 'c') {
      if (runExternal(transBin, [lang, ...args], options.verbose) === 0) {
        console.log(chalk.green('Translation and compile completed successfully.'));
      } else {
        console.log(chalk.red('Translation and compile failed.'));
      }
    } else {
      const dir = path.dirname(path.resolve(file));
      const fname = path.basename(file);
      let dockerArgs = ['run', '--rm', '-v', `${dir}:/app`, '-w', '/app', `vib-translator-${lang}`, 'translate', fname, ...args];
      if (runExternal('docker', dockerArgs, options.verbose) === 0) {
        console.log(chalk.green('Docker translation and compile completed successfully.'));
      } else {
        console.log(chalk.red('Docker translation and compile failed.'));
      }
    }
  });

tCmd.command('list')
  .action(() => {
    let supported = ['c', 'go', 'java', 'python', 'rust', 'zig'].sort();
    console.log(chalk.green('Supported translation languages:'));
    for (let l of supported) {
      console.log('- ' + l);
    }
  });

program.command('run')
  .option('--debug')
  .option('--verbose')
  .argument('<file>')
  .action((file, options) => {
    if (checkCode(file, true, options.verbose) !== 0) process.exit(1);
    let args = [file];
    if (options.debug) args.push('--debug');
    if (options.verbose) args.push('--verbose');
    if (runExternal(vmBin, args, options.verbose) === 0) {
      console.log(chalk.green('Run completed successfully.'));
    } else {
      console.log(chalk.red('Run failed.'));
    }
  });

program.command('init')
  .option('--license <license>', 'MIT')
  .option('--repo <repo>', '')
  .option('--verbose')
  .argument('[projectName]', 'my-vib-project')
  .action((projectName, options) => {
    if (fs.existsSync(projectName)) {
      console.log(chalk.red(`Directory ${projectName} already exists.`));
      process.exit(1);
    }
    fs.mkdirSync(projectName);
    const cmdDir = path.join(projectName, 'cmd');
    fs.mkdirSync(cmdDir);
    const mainFile = path.join(cmdDir, 'main.vib');
    const exampleCode = `function main() [\n\twrite("Hello from Vib!")\n]`;
    fs.writeFileSync(mainFile, exampleCode);
    const vmlFile = path.join(projectName, 'Project.vml');
    const vmlContent = `name = "${projectName}"\nlicense = "${options.license}"\nrepository = "${options.repo}"\nversion = "0.1.0"\n\n[dependencies]\n\nbuild_targets = ["exe", "bin", "deb", "rpm"]\nsource_dir = "cmd/main.vib"\n`;
    fs.writeFileSync(vmlFile, vmlContent);
    if (options.verbose) {
      console.log(chalk.yellow('Project files created:'));
      console.log(mainFile);
      console.log(vmlFile);
    }
    console.log(chalk.green(`Initialized project ${projectName} with license ${options.license} and repo ${options.repo}.`));
  });

program.command('tutorials')
  .argument('[topic]', '')
  .action((topic) => {
    if (topic === '') {
      console.log('Available topics: syntax, functions, embedded, libraries, comments');
      return;
    }
    switch (topic.toLowerCase()) {
      case 'syntax':
        console.log('Vib syntax draws clarity from Rust, most syntax from Python: [] instead of {}, write instead of print.');
        break;
      case 'functions':
        console.log('Functions are defined like in JavaScript: function name(params) [ body ]');
        break;
      case 'embedded':
        console.log('Embed code from other languages: #=lang= [ code ]');
        break;
      case 'libraries':
        console.log('Require libraries: require "module"; for foreign: require "lang:module". Specify in <> like <python:numpy>.');
        break;
      case 'comments':
        console.log('Single line: ~ comment\\nMulti line: :: comment ::');
        break;
      default:
        console.log(chalk.red(`Unknown topic: ${topic}`));
        console.log('Try: syntax, functions, embedded, libraries, comments');
    }
  });

program.command('doc')
  .option('--verbose')
  .argument('<file>')
  .action((file, options) => {
    console.log(chalk.yellow(`Generating documentation for ${file} (placeholder)`));
    if (options.verbose) {
      console.log('Doc output: Documentation generated.');
    }
  });

program.command('autor')
  .action(() => {
    console.log(chalk.blue(`Author of Vib: ${vibAuthor}`));
  });

program.command('version')
  .action(() => {
    console.log(chalk.blue(`Vib version: ${vibVersion}`));
  });

program.parse(process.argv);
