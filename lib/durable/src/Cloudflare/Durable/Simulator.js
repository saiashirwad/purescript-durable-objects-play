// @ts-check
// The in-memory SQLite behind the test simulator. Node only.
import { DatabaseSync } from "node:sqlite";

/**
 * @typedef {import("node:sqlite").SQLInputValue} SQLInputValue
 * @typedef {import("node:sqlite").SQLOutputValue} SQLOutputValue
 */

/** @type {Effect<DatabaseSync>} */
export const openMemory = () => new DatabaseSync(":memory:");

/**
 * Same rules as `Platform.js`: booleans become 0/1, structures their JSON text.
 *
 * @param {unknown} value
 * @returns {SQLInputValue}
 */
const bindable = (value) =>
  typeof value === "boolean"
    ? value ? 1 : 0
    : value !== null && typeof value === "object"
      ? JSON.stringify(value)
      : /** @type {SQLInputValue} */ (value);

/**
 * @param {DatabaseSync} db
 * @returns {(query: string) => (bindings: unknown[]) => Effect<Record<string, SQLOutputValue>[]>}
 */
export const exec = (db) => (query) => (bindings) => () =>
  db.prepare(query).all(...bindings.map(bindable)).map((row) => ({ ...row }));
/**
 * Run SQL commands atomically, matching Durable Object `transactionSync`.
 *
 * @param {DatabaseSync} db
 * @returns {(commands: { sql: string, bindings: unknown[] }[]) => Effect<Record<string, SQLOutputValue>[][]>}
 */
export const execBatch = (db) => (commands) => () => {
  db.exec("BEGIN IMMEDIATE");
  try {
    const results = commands.map(({ sql, bindings }) =>
      db.prepare(sql).all(...bindings.map(bindable)).map((row) => ({ ...row }))
    );
    db.exec("COMMIT");
    return results;
  } catch (failure) {
    db.exec("ROLLBACK");
    throw failure;
  }
};


/**
 * Drop every table, as `storage.deleteAll` does on the platform.
 *
 * @param {DatabaseSync} db
 * @returns {Effect<void>}
 */
export const dropAll = (db) => () => {
  const tables = db
    .prepare("SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'")
    .all();
  for (const { name } of tables) db.exec(`DROP TABLE "${String(name)}"`);
};
