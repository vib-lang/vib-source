package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "package-manager",
	Short: "Vib Package Manager",
	Long:  `A package manager for the Vib programming language, handling installation, removal, and updates of libraries, including those from other languages.`,
}

var installCmd = &cobra.Command{
	Use:   "install <package>",
	Short: "Install a library locally",
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		pkg := args[0]
		lang, name := parsePackage(pkg)
		if lang == "" || name == "" {
			return fmt.Errorf("invalid package format: %s (use <lang>:<name> or just <name> for vib)", pkg)
		}

		if lang == "vib" {
			// Placeholder for vib packages: create local dir
			installPath, err := getInstallPath(lang, name)
			if err != nil {
				return err
			}
			if err := os.MkdirAll(installPath, 0755); err != nil {
				return fmt.Errorf("failed to install %s: %v", pkg, err)
			}
			// Simulate package content
			if err := os.WriteFile(filepath.Join(installPath, "package.vib"), []byte("// Placeholder Vib package"), 0644); err != nil {
				return fmt.Errorf("failed to create package file: %v", err)
			}
			fmt.Printf("Installed Vib package: %s\n", name)
		} else {
			// Handle other languages
			err := installForeignPackage(lang, name)
			if err != nil {
				return err
			}
			fmt.Printf("Installed %s package: %s\n", lang, name)
		}
		return nil
	},
}

var removeCmd = &cobra.Command{
	Use:   "remove <package>",
	Short: "Remove a local library",
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		pkg := args[0]
		lang, name := parsePackage(pkg)
		if lang == "" || name == "" {
			return fmt.Errorf("invalid package format: %s (use <lang>:<name> or just <name> for vib)", pkg)
		}

		if lang == "vib" {
			installPath, err := getInstallPath(lang, name)
			if err != nil {
				return err
			}
			if err := os.RemoveAll(installPath); err != nil {
				return fmt.Errorf("failed to remove %s: %v", pkg, err)
			}
			fmt.Printf("Removed Vib package: %s\n", name)
		} else {
			err := removeForeignPackage(lang, name)
			if err != nil {
				return err
			}
			fmt.Printf("Removed %s package: %s\n", lang, name)
		}
		return nil
	},
}

var updateCmd = &cobra.Command{
	Use:   "update [package]",
	Short: "Update a package or the Vib language (placeholder)",
	RunE: func(cmd *cobra.Command, args []string) error {
		if len(args) == 0 {
			// Update Vib itself (placeholder)
			fmt.Println("Updating Vib language... (placeholder implementation)")
			// TODO: Implement actual update logic, e.g., git pull or download new version
			return nil
		}

		pkg := args[0]
		lang, name := parsePackage(pkg)
		if lang == "" || name == "" {
			return fmt.Errorf("invalid package format: %s (use <lang>:<name> or just <name> for vib)", pkg)
		}

		if lang == "vib" {
			// Placeholder for updating vib packages
			fmt.Printf("Updating Vib package %s... (placeholder)\n", name)
		} else {
			err := updateForeignPackage(lang, name)
			if err != nil {
				return err
			}
			fmt.Printf("Updated %s package: %s\n", lang, name)
		}
		return nil
	},
}

func main() {
	rootCmd.AddCommand(installCmd)
	rootCmd.AddCommand(removeCmd)
	rootCmd.AddCommand(updateCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// parsePackage parses the package specifier, e.g., "python:numpy" or "numpy" (defaults to vib)
func parsePackage(pkg string) (lang, name string) {
	parts := strings.SplitN(pkg, ":", 2)
	if len(parts) == 2 {
		return strings.ToLower(parts[0]), parts[1]
	}
	return "vib", pkg
}

// getInstallPath returns the local install path for vib packages
func getInstallPath(lang, name string) (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("failed to get home directory: %v", err)
	}
	return filepath.Join(home, ".vib", "lib", lang, name), nil
}

// installForeignPackage installs packages from other languages (simple exec for supported langs)
func installForeignPackage(lang, name string) error {
	switch lang {
	case "python":
		cmd := exec.Command("pip", "install", name)
		output, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Errorf("failed to install python package %s: %v\n%s", name, err, output)
		}
	case "go":
		cmd := exec.Command("go", "get", name)
		output, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Errorf("failed to install go package %s: %v\n%s", name, err, output)
		}
	// Add more languages as needed, e.g., npm for js, mvn for java
	default:
		return fmt.Errorf("unsupported language: %s (placeholder, extend as needed)", lang)
	}
	return nil
}

// removeForeignPackage removes packages from other languages
func removeForeignPackage(lang, name string) error {
	switch lang {
	case "python":
		cmd := exec.Command("pip", "uninstall", "-y", name)
		output, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Errorf("failed to remove python package %s: %v\n%s", name, err, output)
		}
	case "go":
		// Go doesn't have a direct uninstall, perhaps remove from go.mod or warn
		fmt.Printf("Warning: Go packages are managed via go.mod, manual removal may be needed for %s\n", name)
		return nil
	default:
		return fmt.Errorf("unsupported language: %s", lang)
	}
	return nil
}

// updateForeignPackage updates packages from other languages
func updateForeignPackage(lang, name string) error {
	switch lang {
	case "python":
		cmd := exec.Command("pip", "install", "--upgrade", name)
		output, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Errorf("failed to update python package %s: %v\n%s", name, err, output)
		}
	case "go":
		cmd := exec.Command("go", "get", "-u", name)
		output, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Errorf("failed to update go package %s: %v\n%s", name, err, output)
		}
	default:
		return fmt.Errorf("unsupported language: %s", lang)
	}
	return nil
}
