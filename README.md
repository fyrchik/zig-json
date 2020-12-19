# JSON validator in Zig

My goal was to write JSON validator which:
1. Fully conforms to RFC8259.
2. Passes all tests from http://www.json.org/JSON_checker/ .
3. Passes all tests from https://github.com/nst/JSONTestSuite .
4. If used with the strictest set of options fails all tests started with `i_` (these are JSON files which does not strictly conform to standard but can be accepted).
5. Passes all tests from (4) if used with the most tolerant set of options.
6. Learn Zig along the way.

Non-goals:
1. Replace parser from standard library.
2. Implement streaming parser.

So far each goal has been achieved, with 2 exceptions (I have no plans to fix this):
1. In (5) tests containing BOM are failed. Working with strings in Zig is not pleasant, so I skipped it.
2. In (2) some tests are skipped, because [RFC8259](https://tools.ietf.org/html/rfc8259) allows a bit more than previous [RFC7159](https://tools.ietf.org/html/rfc7159) .
3. There some changes in Errata of RFC8250 which could be taken into account.

This also needs some more testing.
