Runbook:

If developing in VSCode, use calva extension, either "Jack in" or run "Copy Jack In Command Line to Clipboard", run that with the shadow-cljs and :sudoku options, then use the interactive repl

If not in VSCode, use `pnpm run watch` for development and `pnpm run repl` to start the repl

Tailwind used for styling - use `pnpm run tailwind` to generate the output css. Run this with `--watch` to update CSS during development

# Sudoku App Roadmap

## ✅ 1. Project Initialization (Completed)
- Initialize project with `shadow-cljs`, `reagent`, and `tailwindcss`.
- Confirm basic rendering of a reagent component styled with TailwindCSS.

---

## 📌 2. Core Structure & UI Layout
- **2.1** Set up a simple UI with a Sudoku grid (9x9).
- **2.2** Style the grid using TailwindCSS, making sure cells are clickable.
- **2.3** Implement basic styling for active/selected cells.

---

## 📌 3. State Management & Reagent Integration
- **3.1** Define application state using Reagent's `atom`.
- **3.2** Implement a mechanism to update state when a cell is clicked or a number is entered.
- **3.3** Render updates to the grid based on state changes.

---

## 📌 4. Sudoku Logic Implementation
- **4.1** Create functions for:
  - Checking row, column, and sub-grid validity.
  - Detecting when the puzzle is solved.
- **4.2** Implement a generator for valid Sudoku puzzles (start simple with pre-defined puzzles).

---

## 📌 5. Interaction Handling
- **5.1** Add keyboard input handling for number entry.
- **5.2** Implement click-to-select cells and highlight active cell.
- **5.3** Add features like clear cell, undo, and redo.

---

## 📌 6. Game Functionality & Features
- **6.1** Add difficulty levels (Easy, Medium, Hard) via different puzzle generators.
- **6.2** Implement a timer to track completion time.
- **6.3** Add hints functionality.
- **6.4** Validate puzzles as the user enters numbers (instant or on-demand validation).

---

## 📌 7. Styling & UX Improvements
- **7.1** Enhance styling with TailwindCSS for better visuals and responsiveness.
- **7.2** Add animations and transitions for interactions (e.g., cell selection, errors).
- **7.3** Implement dark mode toggle.

---

## 📌 8. Persistence & Storage
- **8.1** Implement local storage to save progress.
- **8.2** Allow resuming saved puzzles on reload.

---

## 📌 9. Testing & Optimization
- **9.1** Add unit tests for core logic (validity checks, puzzle generation, etc.).
- **9.2** Profile and optimize rendering for smooth experience.

---

## 📌 10. Deployment
- **10.1** Build the project using `shadow-cljs`.
- **10.2** Deploy to a static site host (e.g., GitHub Pages, Vercel, Netlify).

---

## 📌 11. Extra Features (Optional)
- Daily puzzles.
- User authentication and leaderboards.
- Custom puzzle creation.
