import { DatabaseSync } from "node:sqlite";

export const openMemory = () => new DatabaseSync(":memory:");

const bindable = (value) =>
  typeof value === "boolean"
    ? value ? 1 : 0
    : value !== null && typeof value === "object"
      ? JSON.stringify(value)
      : value;

export const exec = (db) => (query) => (bindings) => () =>
  db.prepare(query).all(...bindings.map(bindable)).map((row) => ({ ...row }));

export const dropAll = (db) => () => {
  const tables = db
    .prepare("SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'")
    .all();
  for (const { name } of tables) db.exec(`DROP TABLE "${name}"`);
};
