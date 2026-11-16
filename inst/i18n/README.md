# Languages of CycloDesk

CycloDesk is written in English. Each other language is one file here, `<code>.tsv` (`fr.tsv`, `nl.tsv`, `de.tsv`,
`es.tsv`, `zh.tsv`), with two tab-separated columns: the English text and its translation.

```
# language: Français
File	Fichier
Save figure	Enregistrer la figure
```

* The file is UTF-8. Lines that start with `#` are comments; `# language: <name>` is the name shown in
  **Help > Language**.
* A text that is not in the file stays English, so a translation may be partial.
* Keep `%s` and `%d` (they are replaced by a value), and write `\n` for a line break.
* Names of methods, packages and functions (TimeOpt, COCO, MTM, WaverideR, ...) and units stay as they are.
* Figures, result texts, the R code and the log are not translated.

## Adding or improving a language

1. Copy `texts_en.txt` (every English text of the interface, one per line) and put the translation after a tab on
   each line; save it as `<two-letter code>.tsv` with a `# language:` line at the top. To improve a language, edit
   its file.
2. Install the package again. The language appears in **Help > Language**; CycloDesk starts in the language of the
   computer when there is a file for it.

## Updating `texts_en.txt` after the interface changed

Start CycloDesk with `options(CycloDesk.i18n_collect = "seen.txt")` and open the tools, tabs and dialogs: every text
that passes through the translation is written to that file once. Texts are translated where they are shown
(`cd_tr_tags()` in `R/i18n.R`), so tools need no code for translation.
