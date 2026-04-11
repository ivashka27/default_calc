#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr
#include <string>


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
    const auto rollback = [&i, &line] (const std::size_t n) {
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

std::size_t skip_ws(const std::string& line, std::size_t i) {
    while (i < line.size() &&
           std::isspace(static_cast<unsigned char>(line[i]))) {
        ++i;
    }
    return i;
}


int char_to_digit(const char c) {
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


double parse_positive_number_with_base(const std::string& line,
                                       std::size_t& i,
                                       const int base) {
    double result = 0.0;
    bool seen_digit = false;
    bool seen_dot = false;
    std::size_t int_digits = 0;
    std::size_t frac_digits = 0;
    double frac_multiplier = 1.0;

    while (i < line.size()) {
        if (line[i] == '.') {
            if (seen_dot) {
                std::cerr << "Argument parsing error at " << i
                          << ": second decimal point" << std::endl;
                return result;
            }
            seen_dot = true;
            ++i;
            continue;
        }

        const int digit = char_to_digit(line[i]);
        if (digit < 0 || digit >= base) {
            break;
        }

        seen_digit = true;
        if (!seen_dot) {
            if (int_digits >= max_decimal_digits) {
                std::cerr << "Argument has more than " << max_decimal_digits
                          << " digits" << std::endl;
                return result;
            }
            result = result * base + digit;
            ++int_digits;
        } else {
            if (frac_digits >= max_decimal_digits) {
                std::cerr << "Argument has more than " << max_decimal_digits
                          << " digits after decimal point" << std::endl;
                return result;
            }
            frac_multiplier /= base;
            result += digit * frac_multiplier;
            ++frac_digits;
        }
        ++i;
    }

    if (!seen_digit) {
        std::cerr << "Argument parsing error at " << i
                  << ": expected digits" << std::endl;
    }

    return result;
}



double parse_arg(const std::string& line, std::size_t& i) {
    const std::size_t start = i;

    if (i >= line.size()) {
        std::cerr << "Argument parsing error at " << i
                  << ": empty argument" << std::endl;
        return 0.0;
    }

    int base = 10;

    if (line[i] == '0') {
        if (i + 1 < line.size() && (line[i + 1] == 'b' || line[i + 1] == 'B')) {
            base = 2;
            i += 2;
        } else if (i + 1 < line.size() && (line[i + 1] == 'x' || line[i + 1] == 'X')) {
            base = 16;
            i += 2;
        } else if (i + 1 < line.size() && line[i + 1] != '.') {
            base = 8;
        }
    }

    const std::size_t digits_start = i;
    const double result = parse_positive_number_with_base(line, i, base);

    if (i == digits_start) {
        return 0.0;
    }

    if (i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '"
                  << line.substr(i) << "'" << std::endl;
    }

    if (i == start) {
        std::cerr << "Argument parsing error at " << i << std::endl;
        return 0.0;
    }

    return result;
}


//izmenil chtob mojno bilo izvlech koren' iz 0
double unary(const double current, const Op op) {
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
            }
            else {
                std::cerr << "Bad right argument for division: " << right << std::endl;
                return left;
            }
        case Op::REM:
            if (right != 0) {
                return std::fmod(left, right);
            }
            else {
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
    if (line.empty()) {
        std::cerr << "Empty input" << std::endl;
        return current;
    }

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
                        break;
                    }
                    return binary(op, current, arg);
                }
        case 1: {
                    if (i < line.size()) {
                        std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
                        break;
                    }
                    return unary(current, op);
                }
        default: break;
    }
    return current;
}