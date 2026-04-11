#include "calc.hpp"

#include <cctype>
#include <cmath>
#include <iostream>

namespace {

const std::size_t max_decimal_digits = 10;

enum class Op {
    ERR,
    SET,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    NEG,
    POW,
    SQRT,
    FACT,
    LOG
};

std::size_t skip_ws(const std::string &line, std::size_t i) {
    while (i < line.size() && std::isspace(static_cast<unsigned char>(line[i]))) {
        ++i;
    }
    return i;
}

std::size_t arity(const Op op) {
    switch (op) {
        case Op::NEG:
        case Op::SQRT:
        case Op::FACT:
            return 1;

        case Op::SET:
        case Op::ADD:
        case Op::SUB:
        case Op::MUL:
        case Op::DIV:
        case Op::REM:
        case Op::POW:
        case Op::LOG:
            return 2;

        case Op::ERR:
        default:
            return 0;
    }
}

Op parse_op(const std::string &line, std::size_t &i) {
    const auto fail = [&]() {
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };

    if (i >= line.size()) {
        return fail();
    }

    if (std::isdigit(static_cast<unsigned char>(line[i]))) {
        return Op::SET;
    }

    if (line.compare(i, 4, "SQRT") == 0) {
        i += 4;
        return Op::SQRT;
    }

    if (line.compare(i, 3, "LOG") == 0) {
        i += 3;
        return Op::LOG;
    }

    switch (line[i]) {
        case '+':
            ++i;
            return Op::ADD;
        case '-':
            ++i;
            return Op::SUB;
        case '*':
            ++i;
            return Op::MUL;
        case '/':
            ++i;
            return Op::DIV;
        case '%':
            ++i;
            return Op::REM;
        case '_':
            ++i;
            return Op::NEG;
        case '^':
            ++i;
            return Op::POW;
        case '!':
            ++i;
            return Op::FACT;
        default:
            return fail();
    }
}

bool parse_arg_token(const std::string &line, std::size_t &i, double &res) {
    res = 0.0;
    std::size_t count = 0;
    bool saw_digit = false;
    bool saw_dot = false;
    double fraction = 1.0;

    while (i < line.size() && count < max_decimal_digits) {
        const char c = line[i];

        if (std::isdigit(static_cast<unsigned char>(c))) {
            saw_digit = true;
            if (!saw_dot) {
                res *= 10.0;
                res += static_cast<double>(c - '0');
            } else {
                fraction /= 10.0;
                res += static_cast<double>(c - '0') * fraction;
            }
            ++i;
            ++count;
        } else if (c == '.' && !saw_dot) {
            saw_dot = true;
            ++i;
        } else {
            break;
        }
    }

    if (!saw_digit) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        return false;
    }

    if (i < line.size() && !std::isspace(static_cast<unsigned char>(line[i]))) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        return false;
    }

    return true;
}

bool apply_unary(const double current, const Op op, double &result) {
    switch (op) {
        case Op::NEG:
            result = -current;
            return true;

        case Op::SQRT:
            if (current >= 0.0) {
                result = std::sqrt(current);
                return true;
            }
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            return false;

        case Op::FACT:
            if (current < 0.0 || std::floor(current) != current) {
                std::cerr << "Bad argument for factorial: " << current << std::endl;
                return false;
            }

            result = 1.0;
            for (unsigned long long k = 2; k <= static_cast<unsigned long long>(current); ++k) {
                result *= static_cast<double>(k);
            }
            return true;

        default:
            result = current;
            return false;
    }
}

bool apply_binary(const Op op, const double left, const double right, double &result) {
    switch (op) {
        case Op::SET:
            result = right;
            return true;

        case Op::ADD:
            result = left + right;
            return true;

        case Op::SUB:
            result = left - right;
            return true;

        case Op::MUL:
            result = left * right;
            return true;

        case Op::DIV:
            if (right != 0.0) {
                result = left / right;
                return true;
            }
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return false;

        case Op::REM:
            if (right != 0.0) {
                result = std::fmod(left, right);
                return true;
            }
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return false;

        case Op::POW:
            result = std::pow(left, right);
            return true;

        case Op::LOG:
            if (left <= 0.0) {
                std::cerr << "Bad argument for logarithm: " << left << std::endl;
                return false;
            }
            if (right <= 0.0 || right == 1.0) {
                std::cerr << "Bad base for logarithm: " << right << std::endl;
                return false;
            }
            result = std::log(left) / std::log(right);
            return true;

        default:
            result = left;
            return false;
    }
}

} // namespace

double process_line(const double current, const std::string &line) {
    std::size_t i = skip_ws(line, 0);

    // bonus
    if (i < line.size() && line[i] == '(') {
        ++i;
        i = skip_ws(line, i);

        const auto op = parse_op(line, i);
        if (op == Op::ERR) {
            return current;
        }

        if (arity(op) != 2 || op == Op::SET) {
            std::cerr << "Unknown operation " << line << std::endl;
            return current;
        }

        i = skip_ws(line, i);
        if (i >= line.size() || line[i] != ')') {
            std::cerr << "Unknown operation " << line << std::endl;
            return current;
        }
        ++i;

        double acc = current;

        while (true) {
            i = skip_ws(line, i);
            if (i == line.size()) {
                return acc;
            }

            double arg = 0.0;
            if (!parse_arg_token(line, i, arg)) {
                return current;
            }

            double next = acc;
            if (!apply_binary(op, acc, arg, next)) {
                return current;
            }
            acc = next;
        }
    }
    
    const auto op = parse_op(line, i);

    switch (arity(op)) {
        case 2: {
            i = skip_ws(line, i);
            if (i == line.size()) {
                std::cerr << "No argument for a binary operation" << std::endl;
                return current;
            }

            double arg = 0.0;
            if (!parse_arg_token(line, i, arg)) {
                return current;
            }

            i = skip_ws(line, i);
            if (i < line.size()) {
                std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
                return current;
            }

            double result = current;
            if (apply_binary(op, current, arg, result)) {
                return result;
            }
            return current;
        }

        case 1: {
            i = skip_ws(line, i);
            if (i < line.size()) {
                std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
                return current;
            }

            double result = current;
            if (apply_unary(current, op, result)) {
                return result;
            }
            return current;
        }

        default:
            return current;
    }
}