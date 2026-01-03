use std::path::{Path, PathBuf};
use std::{env, error::Error, fs, io::Read, process::Command};

fn main() {
    println!("cargo:rerun-if-changed=src/parser.lalrpop");

    let grammar = PathBuf::from("src/parser.lalrpop");
    let (result, stdout, stderr) = capture_stdio(|| {
        lalrpop::Configuration::new()
            .emit_rerun_directives(true)
            .process_file(&grammar)
    });

    if let Err(error) = result {
        handle_lalrpop_error(&*error, &stdout, &stderr);
    }
}

fn handle_lalrpop_error(error: &dyn Error, stdout: &str, stderr: &str) {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .expect("CARGO_MANIFEST_DIR not set");

    let log_path = manifest_dir.join("log.txt");
    let cleaned_path = manifest_dir.join("cleaned_errores.txt");

    // Guardar log completo
    let combined_log = format!(
        "=== lalrpop::process_file returned Err ===\n{error}\n\n\
         === captured stdout ===\n{stdout}\n\n\
         === captured stderr ===\n{stderr}\n"
    );
    fs::write(&log_path, combined_log).ok();

    // Procesar con script Python si existe
    clean_log_with_python(&manifest_dir, &log_path, &cleaned_path);

    // Emitir warning y panic
    println!(
        "cargo:warning=LALRPOP falló; log en {} y cleaned en {}",
        log_path.display(),
        cleaned_path.display()
    );
    panic!("LALRPOP failed: {error}");
}

fn clean_log_with_python(manifest_dir: &Path, log_path: &Path, cleaned_path: &Path) {
    let python_script = manifest_dir.join("compact_lalrpop_v4.py");

    if let Ok(output) = Command::new("python")
        .arg(python_script)
        .arg(log_path)
        .output()
    {
        fs::write(cleaned_path, output.stdout).ok();
    }
}

#[rustfmt::skip]
fn capture_stdio<F, T>(f: F) -> (T, String, String)
where
    F: FnOnce() -> T,
{
    let mut stdout_redirect = gag::BufferRedirect::stdout()
        .expect("failed to redirect stdout");
    let mut stderr_redirect = gag::BufferRedirect::stderr()
        .expect("failed to redirect stderr");

    let result = f();

    let mut stdout = String::new();
    let mut stderr = String::new();
    stdout_redirect.read_to_string(&mut stdout).ok();
    stderr_redirect.read_to_string(&mut stderr).ok();

    (result, stdout, stderr)
}
