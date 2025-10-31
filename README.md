# roster_methods

This repository uses a simple numeric-prefixed folder layout for reproducible data and scripts.

Folder structure created:

- `00_data` – raw and processed datasets. Keep original raw files here (do not modify originals).
- `01_scripts` – analysis, preprocessing, and utility scripts.
- `02_output` – generated outputs such as figures, tables, and intermediate results.
- `03_docs` – documentation, notes, and reports.

Notes:
- Numeric prefixes enforce ordering. Keep them.
- Consider adding `.gitignore` entries to exclude large files in `02_output` if needed.
- You can add `.gitkeep` files to ensure empty directories are tracked by git.

Created by automation to initialize the project layout.
