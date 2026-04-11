#include "calc.hpp"

#include <cctype>   // for std::isspace
#include <cmath>    // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;

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

std::size_t arity(const Op op)
{
    switch (op) {
        case Op::ERR: return 0;
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
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
    const auto rollback = [&i, &line](const std::size_t n) {
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
            --i;
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

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

double digit_value(char c)
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

double parse_with_base(const std::string& line, std::size_t& i, int base)
{
    double res = 0;
    bool integer = true;
    double fraction = 1;

    while (i < line.size()) {
        if (line[i] == '.') {
            integer = false;
            ++i;
            continue;
        }

        double digit = digit_value(line[i]);

        if (digit < 0 || digit >= base) {
            break;
        }

        if (integer) {
            res = res * base + digit;
        } else {
            fraction /= base;
            res += digit * fraction;
        }

        ++i;
    }

    return res;
}

double parse_arg(const std::string & line, std::size_t & i)
{
    // binary: 0b101.11
    if (i + 1 < line.size() &&
        line[i] == '0' &&
        (line[i + 1] == 'b' || line[i + 1] == 'B')) {
        i += 2;
        return parse_with_base(line, i, 2);
    }

    // hexadecimal: 0xFF.1C
    if (i + 1 < line.size() &&
        line[i] == '0' &&
        (line[i + 1] == 'x' || line[i + 1] == 'X')) {
        i += 2;
        return parse_with_base(line, i, 16);
    }

    // octal: 01347.7
    if (i + 1 < line.size() &&
        line[i] == '0' &&
        line[i + 1] != '.' &&
        line[i + 1] != 'x' &&
        line[i + 1] != 'X' &&
        line[i + 1] != 'b' &&
        line[i + 1] != 'B') {
        ++i;
        return parse_with_base(line, i, 8);
    }

    // decimal
    return parse_with_base(line, i, 10);
}

double unary(const double current, const Op op)
{
    switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current >= 0) {
                return std::sqrt(current);
            } else {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                return current;
            }
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
            } else {
                std::cerr << "Bad right argument for division: " << right << std::endl;
                return left;
            }
        case Op::REM:
            if (right != 0) {
                return std::fmod(left, right);
            } else {
                std::cerr << "Bad right argument for remainder: " << right << std::endl;
                return left;
            }
        case Op::POW:
            return std::pow(left, right);
        default:
            return left;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    const auto op = parse_op(line, i);

    switch (arity(op)) {
        case 2: {
            i = skip_ws(line, i);
            const auto old_i = i;
            const auto arg = parse_arg(line, i);

            if (i == old_i) {
                std::cerr << "No argument for a binary operation" << std::endl;
                break;
            }
            else if (i < line.size()) {
                std::cerr << "Argument isn't fully parsed, suffix left: '"
                          << line.substr(i) << "'" << std::endl;
                break;
            }

            return binary(op, current, arg);
        }

        case 1: {
            if (i < line.size()) {
                std::cerr << "Unexpected suffix for a unary operation: '"
                          << line.substr(i) << "'" << std::endl;
                break;
            }
            return unary(current, op);
        }

        default:
            break;
    }

    return current;
}
