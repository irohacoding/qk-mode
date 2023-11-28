# Changelog

## main (Unreleased)

### Modified
- favorite phrase to favorite words.

## v0.2.0 (2023-11-28)

### Changed
- Favorite word to coffee cup with steam in center.
- defgroup :group from tools to applications.

### Added
- qk-mode--insert-favorite-quote function.
- autoload definition to qk function.
- qk-mode-show-phrase for defcustom.
- defcustom for favorite phrase and max steaming count.
  - qk-mode-max-steaming-count.
  - qk-mode-favorite-phrase.

### Modified
- timing of insert phrase.
- function names to added double hyphen.

### Fixed
- qk-insert-steam function.
  - delete-line to delete-region.
- Last line comment.
  - Add .el extension.
  - Remove period.
- prefix string "qk-" to "qk-mode-".
- Add period for comment of qk-mode-buffer.
- descrepancy of coffee cup when running in terminal.
- deleted white space on Commentary: section.

### Removed
- defconst qk-mode-version.

## v0.1.0 (2023-05-26)

- First release.
  - `M-x qk` to launch QK Mode.
  - Display favorite word in \*break\* buffer.
  - Favorite word can customize to yours.
