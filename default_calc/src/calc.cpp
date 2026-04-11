#include "calc.hpp"

#include <cctype> // for std::isspace, std::tolower
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr
#include <string>

namespace {

const std::size_t max_digits = 10;

enum class Op {
      ERR
    , SET
    , ADD
    , SUB
    , MUL
    , DIV
    , REM
    , NEG
    , POW
    , SQRT
};

struct ParseResult {
    double value = 0;
    bool ok = false;
};

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(static_cast<unsigned char>(line[i]))) {
        ++i;
    }
    return i;
}

std::size_t arity(const Op op)
{
    switch (op) {
        // error
        case Op::ERR: return 0;
        // unary
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
        // binary
        case Op::SET: return 2;
        case Op::ADD: return 2;
        case Op::SUB: return 2;
        case Op::MUL: return 2;
        case Op::DIV: return 2;
        case Op::REM: return 2;
        case Op::POW: return 2;
    }
    return 0;
}

Op parse_op(const std::string & line, std::size_t & i)
{
    const auto rollback = [&line, &i] (const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };

    switch (line[i++]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            --i; // a first digit is a part of op's argument
            return Op::SET;
        case '+':
            return Op::ADD;
        case '-':
            return Op::SUB;
        case '*':
            return Op::MUL;
        case '/':
            return Op::DIV;
        case '%':
            return Op::REM;
        case '_':
            return Op::NEG;
        case '^':
            return Op::POW;
        case 'S':
            switch (line[i++]) {
                case 'Q':
                    switch (line[i++]) {
                        case 'R':
                            switch (line[i++]) {
                                case 'T':
                                    return Op::SQRT;
                                default:
                                    return rollback(4);
                            }
                        default:
                            return rollback(3);
                    }
                default:
                    return rollback(2);
            }
        default:
            return rollback(1);
    }
}

bool is_octal_digit(const char c)
{
    return c >= '0' && c <= '7';
}

// decode numbers from string
int decode_digit(const char c)
{
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'a' && c <= 'f') {
        return 10 + (c - 'a');
    }
    if (c >= 'A' && c <= 'F') {
        return 10 + (c - 'A');
    }
    return -1;
}

ParseResult parse_arg(const std::string & line, std::size_t & i)
{
    const std::size_t begin = i;
    std::size_t base = 10;
    std::size_t digits = 0;
    bool saw_digit = false;

    // Determine which numeral system is used by the argument prefix.
    if (i < line.size() && line[i] == '0' && i + 1 < line.size()) {
        const char next = static_cast<char>(std::tolower(static_cast<unsigned char>(line[i + 1])));
        if (next == 'b') {
            base = 2;
            i += 2;
        }
        else if (next == 'x') {
            base = 16;
            i += 2;
        }
        else if (is_octal_digit(line[i + 1])) {
            base = 8;
            ++i;
            saw_digit = true;
            digits = 1;
        }
    }

    double value = 0;
    bool integer_part = true;
    double fraction = 1;

    while (i < line.size()) {
        if (line[i] == '.') {
            if (!integer_part) {
                std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
                return {};
            }
            integer_part = false;
            ++i;
            continue;
        }

        const int digit = decode_digit(line[i]);
        if (digit < 0 || static_cast<std::size_t>(digit) >= base) {
            break;
        }
        if (digits >= max_digits) {
            break;
        }

        saw_digit = true;
        ++digits;
        if (integer_part) {
            // Integer digits are accumulated in the detected base.
            value = value * static_cast<double>(base) + digit;
        }
        else {
            // Fractional digits use negative powers of the same base.
            fraction /= static_cast<double>(base);
            value += digit * fraction;
        }
        ++i;
    }

    if (!saw_digit) {
        std::cerr << "Argument parsing error at " << begin << ": '" << line.substr(begin) << "'" << std::endl;
        return {};
    }

    return {value, true};
}

double unary(const double current, const Op op)
{
    switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current >= 0) {
                return std::sqrt(current);
            }
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            return current;
        default:
            return current;
    }
}

double binary(const Op op, const double left, const double right)
{
    switch (op) {
        case Op::SET:
            return right;
        case Op::ADD:
            return left + right;
        case Op::SUB:
            return left - right;
        case Op::MUL:
            return left * right;
        case Op::DIV:
            if (right != 0) {
                return left / right;
            }
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return left;
        case Op::REM:
            if (right != 0) {
                return std::fmod(left, right);
            }
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return left;
        case Op::POW:
            return std::pow(left, right);
        default:
            return left;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = skip_ws(line, 0);
    if (i >= line.size()) {
        std::cerr << "Unknown operation " << line << std::endl;
        return current;
    }

    const Op op = parse_op(line, i);
    switch (arity(op)) {
        case 2: {
            i = skip_ws(line, i);
            const std::size_t arg_begin = i;
            const ParseResult arg = parse_arg(line, i);
            if (!arg.ok || i == arg_begin) {
                std::cerr << "No argument for a binary operation" << std::endl;
                return current;
            }
            i = skip_ws(line, i);
            if (i < line.size()) {
                std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
                return current;
            }
            return binary(op, current, arg.value);
        }
        case 1:
            i = skip_ws(line, i);
            if (i < line.size()) {
                std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
                return current;
            }
            return unary(current, op);
        default:
            return current;
    }
}
