#include "../include/calc.hpp" // указал путь до калк.хпп

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;
const double pi = 3.14159265358979323846;
const double epsilon = 1e-10;
bool rad_on = true;

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
    , SIN
    , COS
    , TAN
    , CTN
    , ASIN
    , ACOS
    , ATAN
    , ACTN
    , RAD
    , DEG

};

double binary(Op op, double left, double right);

std::size_t arity(const Op op)
{
    switch (op) {
        // error
        case Op::ERR: return 0;
        // unary
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
        case Op::SIN: return 1;
        case Op::COS: return 1;
        case Op::TAN: return 1;
        case Op::CTN: return 1;
        case Op::ASIN: return 1;
        case Op::ACOS: return 1;
        case Op::ATAN: return 1;
        case Op::ACTN: return 1;
        case Op::RAD: return 1;
        case Op::DEG: return 1;
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
                    case 'I':
                        switch (line[i++]) {
                            case 'N':
                                return Op::SIN;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'C':
                switch (line[i++]) {
                    case 'O':
                        switch (line[i++]) {
                            case 'S':
                                return Op::COS;
                            default:
                                return rollback(3);
                        }
                    case 'T':
                        switch (line[i++]) {
                            case 'N':
                                return Op::CTN;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'T':
                switch (line[i++]) {
                    case 'A':
                        switch (line[i++]) {
                            case 'N':
                                return Op::TAN;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'A':
                switch (line[i++]) {
                    case 'S':
                        switch (line[i++]) {
                            case 'I':
                                switch (line[i++]) {
                                    case 'N':
                                        return Op::ASIN;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    case 'C':
                        switch (line[i++]) {
                            case 'O':
                                switch (line[i++]) {
                                    case 'S':
                                        return Op::ACOS;
                                    default:
                                        return rollback(4);
                                }
                            case 'T':
                                switch (line[i++]) {
                                    case 'N':
                                        return Op::ACTN;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    case 'T':
                        switch (line[i++]) {
                            case 'A':
                                switch (line[i++]) {
                                    case 'N':
                                        return Op::ATAN;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'R':
                switch (line[i++]) {
                    case 'A':
                        switch (line[i++]) {
                            case 'D':
                                return Op::RAD;
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
        case 'D':
                switch (line[i++]) {
                    case 'E':
                        switch (line[i++]) {
                            case 'G':
                                return Op::DEG;
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

// BONUS_TASK: allow left folds for binary operations in the form "(+) 1 2 3".
bool is_foldable_binary_op(const Op op)
{
    switch (op) {
        case Op::ADD:
        case Op::SUB:
        case Op::MUL:
        case Op::DIV:
        case Op::REM:
        case Op::POW:
            return true;
        default:
            return false;
    }
}

double parse_arg(const std::string & line, std::size_t & i)
{
    double res = 0;
    std::size_t count = 0;
    bool good = true;
    bool integer = true;
    double fraction = 1;
    while (good && i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
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
                if (integer) {
                    res *= 10;
                    res += line[i] - '0';
                }
                else {
                    fraction /= 10;
                    res += (line[i] - '0') * fraction;
                }
                ++i;
                ++count;
                break;
            case '.':
                integer = false;
                ++i;
                break;
            default:
                if (std::isspace(line[i])) {
                    good = false;
                }
                else {
                    std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
                    return res;
                }
                break;
        }
    }
    if (i < line.size() && !std::isspace(line[i])) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
    }
    return res;
}

// BONUS_TASK: parse "(<op>)" prefix and map it to the corresponding binary op.
bool parse_fold_op(const std::string & line, std::size_t & i, Op & op)
{
    if (i >= line.size() || line[i] != '(') {
        return false;
    }
    ++i;
    if (i >= line.size()) {
        std::cerr << "No operation inside fold" << std::endl;
        return false;
    }
    switch (line[i++]) {
        case '+':
            op = Op::ADD;
            break;
        case '-':
            op = Op::SUB;
            break;
        case '*':
            op = Op::MUL;
            break;
        case '/':
            op = Op::DIV;
            break;
        case '%':
            op = Op::REM;
            break;
        case '^':
            op = Op::POW;
            break;
        default:
            std::cerr << "Unsupported fold operation" << std::endl;
            return false;
    }
    if (i >= line.size() || line[i] != ')') {
        std::cerr << "Missing ')' after fold operation" << std::endl;
        return false;
    }
    ++i;
    return true;
}

// BONUS_TASK: apply a binary operation left-to-right over all arguments.
double process_fold_line(double current, const std::string & line)
{
    std::size_t i = 0;
    Op op = Op::ERR;
    if (!parse_fold_op(line, i, op) || !is_foldable_binary_op(op)) {
        return current;
    }

    bool has_args = false;
    while (true) {
        i = skip_ws(line, i);
        if (i >= line.size()) {
            break;
        }
        const auto old_i = i;
        const auto arg = parse_arg(line, i);
        if (i == old_i) {
            std::cerr << "Bad fold argument list" << std::endl;
            return current;
        }
        current = binary(op, current, arg);
        has_args = true;
        i = skip_ws(line, i);
    }

    if (!has_args) {
        std::cerr << "Fold requires at least one argument" << std::endl;
        return current;
    }
    return current;
}

double to_radians(const double angle)
{
    return angle * pi / 180.0;
}

double to_degrees(const double angle)
{
    return angle * 180.0 / pi;
}
//вот это
bool is_zero(const double value)
{
    return std::abs(value) < epsilon; 
}
//и это для того чтобы почти 0 обращался в 0 а не в это: 6.12323e-17
double normalize_zero(const double value)
{
    return is_zero(value) ? 0 : value;
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
            else {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                return current;
            }
        case Op::SIN:
            return normalize_zero(std::sin(rad_on ? current : to_radians(current)));
        case Op::COS:
            return normalize_zero(std::cos(rad_on ? current : to_radians(current)));
        case Op::TAN: {
            const auto angle = rad_on ? current : to_radians(current);
            if (is_zero(std::cos(angle))) {
                std::cerr << "Bad argument for TAN: " << current << std::endl;
                return current;
            }
            return normalize_zero(std::tan(angle));
        }
        case Op::CTN: {
            const auto angle = rad_on ? current : to_radians(current);
            if (is_zero(std::sin(angle))) {
                std::cerr << "Bad argument for CTN: " << current << std::endl;
                return current;
            }
            return normalize_zero(std::cos(angle) / std::sin(angle));
        }
        case Op::ASIN:
            if (current < -1 || current > 1) {
                std::cerr << "Bad argument for ASIN: " << current << std::endl;
                return current;
            }
            return normalize_zero(rad_on ? std::asin(current) : to_degrees(std::asin(current)));
        case Op::ACOS:
            if (current < -1 || current > 1) {
                std::cerr << "Bad argument for ACOS: " << current << std::endl;
                return current;
            }
            return normalize_zero(rad_on ? std::acos(current) : to_degrees(std::acos(current)));
        case Op::ATAN:
            return normalize_zero(rad_on ? std::atan(current) : to_degrees(std::atan(current)));
        case Op::ACTN:
            if (is_zero(current)) {
                std::cerr << "Bad argument for ACTN: " << current << std::endl;
                return current;
            }
            return normalize_zero(rad_on ? std::atan(1.0 / current) : to_degrees(std::atan(1.0 / current)));
        case Op::RAD:
            rad_on = true;
            return current;
        case Op::DEG:
            rad_on = false;
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
    auto i = skip_ws(line, 0);
    if (i >= line.size()) {
        return current;
    }
    // BONUS_TASK: route lines like "(+) 1 2 3" into fold processing.
    if (i < line.size() && line[i] == '(') {
        return process_fold_line(current, line.substr(i));
    }
    i = skip_ws(line, 0);
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
                        break;
                    }
                    return binary(op, current, arg);
                }
        case 1: {
                    i = skip_ws(line, i);
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
