use go125_parser::parse_source;
use walkdir::WalkDir;

#[test]
fn parses_go_corpus_if_configured() {
    let Some(root) = std::env::var_os("GO125_PARSER_CORPUS") else {
        eprintln!("GO125_PARSER_CORPUS not set; skipping corpus test");
        return;
    };

    let root = root.to_string_lossy().to_string();
    let mut total = 0usize;
    let mut failed = 0usize;

    for entry in WalkDir::new(&root).into_iter().filter_map(|e| e.ok()) {
        if !entry.file_type().is_file() {
            continue;
        }
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("go") {
            continue;
        }
        // Skip generated / known problematic dirs if desired.
        if path.to_string_lossy().contains("testdata") {
            continue;
        }

        total += 1;
        let src = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => continue,
        };

        if let Err(f) = parse_source(&src) {
            failed += 1;
            eprintln!("FAILED: {}", path.display());
            for d in f.diags.iter().take(8) {
                eprintln!("  {:?} {:?}: {}", d.kind, d.span, d.message);
            }
            // Stop early so failures are fast to triage.
            panic!("Go corpus parse failed after {total} files (failed={failed})");
        }
    }

    eprintln!("Parsed {total} Go files successfully.");
}
