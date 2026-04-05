#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;

const double PI = 3.14159265358979323846; //for calculating trig functions

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
    , SIN //trigonometry
    , COS
    , TAN
    , CTN
    , ASIN
    , ACOS
    , ATAN
    , ACTN
    , RAD //mode
    , DEG
};

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
                good = false;
                break;
        }
    }
    if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
    }
    else if (i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
    }
    return res;
}

//parser for fold function (check out the comment below)
bool parse_arg_fold(const std::string & line, std::size_t & i, double & result)
{
    std::size_t start = i;

    while (i < line.size() && std::isspace(line[i])) { ++i; }

    if (i >= line.size()) { return false; }

    double res = 0;
    std::size_t count = 0;
    bool good = true;
    bool integer = true;
    bool space = false;
    double fraction = 1;
    while (!space && good && i < line.size() && count < max_decimal_digits) {
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
            case ' ':
                space = true;
                ++i;
                break;
            default:
                good = false;
                break;
        }
    }
    if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        return false;
    }

    result = res;
    return true;
}

//extra helpful functions
double to_rad(const double x, const bool rad_on) {
    if (rad_on) { return x; }
    return x * PI / 180.0;
}

double to_deg(const double x, const bool rad_on) {
    if (rad_on) { return x * 180.0 / PI; }
    return x;
}

double normalize_zero(double value) {
    if (std::fabs(value) < 1e-9) { return 0; }
    else { return value; }
}

double unary(const double current, const Op op, bool & rad_on)
{
    const double x = to_rad(current, rad_on);
    switch (op) {
        case Op::NEG:
            return -current;
        case Op::SQRT:
            if (current > 0) { return std::sqrt(current); }
            else {
                std::cerr << "Bad argument for SQRT: " << current << std::endl;
                return current;
            }
        case Op::SIN:
            return normalize_zero(std::sin(x));
        case Op::COS:
            return normalize_zero(std::cos(x));
        case Op::TAN:
            if (normalize_zero(std::cos(x)) != 0) { 
                return normalize_zero(std::sin(x)) / normalize_zero(std::cos(x)); 
            } else {
                std::cerr << "Bad argument for TAN: " << current << std::endl;
                return current;
            }
        case Op::CTN:
            if (normalize_zero(std::sin(x)) != 0) { 
                return normalize_zero(std::cos(x)) / normalize_zero(std::sin(x));
            }
            else {
                std::cerr << "Bad argument for CTN: " << current << std::endl;
                return current;
            }
        case Op::ASIN:
            if (current >= -1 && current <= 1) { return to_deg(std::asin(current), rad_on);}
            else {
                std::cerr << "Bad argument for ASIN: " << current << std::endl;
                return current;
            }
        case Op::ACOS:
            if (current >= -1 && current <= 1) { return to_deg(std::acos(current), rad_on);}
            else {
                std::cerr << "Bad argument for ACOS: " << current << std::endl;
                return current;
            }
        case Op::ATAN:
            return to_deg(std::atan(current), rad_on);
        case Op::ACTN:
            if (current != 0) { return to_deg(std::atan(1.0 / current), rad_on); }
            else {
                std::cerr << "Bad argument for ACTN: " << current << std::endl;
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

//i decided that name "convolution" is not appropriate for this function => called it like that
double fold(const Op op, double current, const std::string & line, std::size_t i)
{
    while (true) {
        i = skip_ws(line, i);
        if (i >= line.size()) { break; }

        const std::size_t prev = i;
        double arg;
        if (!parse_arg_fold(line, i, arg)) { break; }

        current = binary(op, current, arg);
    }
    return current;
}

} // anonymous namespace

double process_line(const double current, bool & rad_on, const std::string & line)
{
    std::size_t i = 0;

    if (!line.empty() && line[0] == '(') {
        ++i;

        const auto op = parse_op(line, i);

        if (op == Op::SET) {
            std::cerr << "Operator SET is not allowed in fold\n";
            return current;
        }

        if (arity(op) != 2) {
            std::cerr << "Only binary operations are allowed\n";
            return current;
        }

        if (i >= line.size() || line[i] != ')') {
            std::cerr << "Expected ')'\n";
            return current;
        }

        ++i; 

        return fold(op, current, line, i);
    }

    const auto op = parse_op(line, i);
    if (op == Op::RAD) {
        rad_on = true;
        return current;
    }
    if (op == Op::DEG) {
        rad_on = false;
        return current;
    }

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
                    return unary(current, op, rad_on);
                }
        default: break;
    }
    return current;
}


/*
Комментарий к бонусному заданию:

мне не нравилось, что программа выводит промежуточные ошибки во время парсинга аргументов
пример:
(+) 1 2 3 4 5
Argument parsing error at 5: ' 2 3 4 5'
Argument parsing error at 7: ' 3 4 5'
Argument parsing error at 9: ' 4 5'
Argument parsing error at 11: ' 5'
15

поэтому я решила сделать отдельный парсер аргументов для fold(), который не будет ругаться на пробелы.
мне показалось это проще и быстрее, чем во все прасеры добавлять дополнительные условия для fold()
*/
