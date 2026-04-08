#include "calc.hpp"

#include <cctype>
#include <cmath>
#include <iostream>
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

    Op parse_op(const std::string& line, std::size_t& i)
    {
        const auto rollback = [&i, &line](const std::size_t n) {
            i -= n;
            std::cerr << "Unknown operation " << line << std::endl;
            return Op::ERR;
            };

        if (i >= line.size()) {
            return Op::ERR;
        }

        if (line[i] == '.') {
            return Op::ERR;
        }

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

    std::size_t skip_ws(const std::string& line, std::size_t i)
    {
        while (i < line.size() && std::isspace(line[i])) {
            ++i;
        }
        return i;
    }

    int digit_value(char c, int base)
    {
        if (c >= '0' && c <= '9') {
            int val = c - '0';
            if (val < base) return val;
            return -1;
        }
        if (base == 16) {
            char lower = std::tolower(c);
            if (lower >= 'a' && lower <= 'f') {
                return 10 + (lower - 'a');
            }
        }
        return -1;
    }

    double parse_number(const std::string& line, std::size_t& i, int base)
    {
        double result = 0.0;
        double frac = 0.0;
        double frac_div = 1.0;
        bool in_frac = false;
        bool has_digits = false;

        while (i < line.size()) {
            char c = line[i];

            if (c == '.') {
                if (in_frac) break;
                in_frac = true;
                ++i;
                continue;
            }

            int val = digit_value(c, base);
            if (val == -1) break;

            has_digits = true;

            if (!in_frac) {
                result = result * base + val;
            }
            else {
                frac_div /= base;
                frac += val * frac_div;
            }
            ++i;
        }

        return has_digits ? result + frac : 0.0;
    }

    double parse_arg(const std::string& line, std::size_t& i)
    {
        while (i < line.size() && std::isspace(line[i])) {
            ++i;
        }

        std::size_t start = i;

        if (i >= line.size()) {
            return 0.0;
        }
        if (line[i] == '.') {
            return 0.0;
        }
        if (line[i] == '0' && i + 1 < line.size() &&
            std::tolower(line[i + 1]) == 'b') {
            i += 2;
            double val = parse_number(line, i, 2);
            if (i == start + 2) {
                std::cerr << "Invalid binary number" << std::endl;
                i = start;
                return 0.0;
            }
            return val;
        }

        if (line[i] == '0' && i + 1 < line.size() &&
            std::tolower(line[i + 1]) == 'x') {
            i += 2;
            double val = parse_number(line, i, 16);
            if (i == start + 2) {
                std::cerr << "Invalid hexadecimal number" << std::endl;
                i = start;
                return 0.0;
            }
            return val;
        }

        if (line[i] == '0') {
            if (i + 1 < line.size() && line[i + 1] == '.') {
                return parse_number(line, i, 10);
            }
            while (i < line.size() && line[i] == '0') {
                ++i;
            }
            if (i < line.size() && line[i] == '.') {
                while (i > start && line[i - 1] == '0') --i;
                return parse_number(line, i, 10);
            }
            double val = parse_number(line, i, 8);
            return val;
        }
        return parse_number(line, i, 10);
    }

    double unary(const double current, const Op op)
    {
        switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current > 0) {
                return std::sqrt(current);
            }
            else {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                [[fallthrough]];
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

double process_line(const double current, const std::string& line)
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

        i = skip_ws(line, i);
        if (i < line.size()) {
            std::cerr << "Unexpected characters after argument: '"
                << line.substr(i) << "'" << std::endl;
            break;
        }

        return binary(op, current, arg);
    }

    case 1: {
        i = skip_ws(line, i);
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
