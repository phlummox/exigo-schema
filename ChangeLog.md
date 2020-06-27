# Changelog for exigo-schema

## Unreleased changes

## 0.2.0.1

- Bug fixes (#2, #7)
- Bounds tightened - requires base >= 4.9

## 0.2.0.0

(Deprecated version, use 0.2.0.1)

- Bug fixes (#1, #3, #4)
- Instances of FromJSON and ToJSON have changed - field names now
  don't repeat the entity name (i.e., `Student` has field `name`,
  rather than `studentName`).

## 0.1.0.0

- initial release