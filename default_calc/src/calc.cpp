#include "../include/calc.hpp" //указал путь до calc.hpp

#include <cctype> // for std::isspace
#include <cmath> // various math functions
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
    , FAC   //добавил оператор факториала
    , LOG   //добавил оператор логарифма   
};

std::size_t arity(const Op op)
{
    switch (op) {
        // error
        case Op::ERR: return 0;
        // unary
        case Op::NEG: return 1;
        case Op::SQRT: return 1;
        case Op::FAC: return 1;     //указал длину операции факториала
        // binary
        case Op::SET: return 2;
        case Op::ADD: return 2;
        case Op::SUB: return 2;
        case Op::MUL: return 2;
        case Op::DIV: return 2;
        case Op::REM: return 2;
        case Op::POW: return 2;
        case Op::LOG: return 2;     //указал длину операции логарифма
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
        case '!':          //распознавание факториала
            return Op::FAC;
        case 'L':          //распознавание логарифма
            switch (line[i++]) {
                case 'O':
                    switch (line[i++]) {
                        case 'G':
                            return Op::LOG;
                        default:
                            return rollback(3);

                    }
                default:
                    return rollback(2);
            }
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

double parse_one_number(const std::string & line, std::size_t & i)
{
    const std::size_t start = i;
    double res = 0;
    std::size_t count = 0;
    bool integer = true;
    double fraction = 1;
    bool any = false;
    while (i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                any = true;
                if (integer) {
                    res *= 10;
                    res += line[i] - '0';
                } else {
                    fraction /= 10;
                    res += (line[i] - '0') * fraction;
                }
                ++i;
                ++count;
                break;
            case '.':
                if (!integer) {
                    goto end_parse_one;
                }
                integer = false;
                ++i;
                break;
            default:
                goto end_parse_one;
        }
    }
end_parse_one:
    if (!any) {
        i = start;
    }
    return res;
}

Op parse_fold_op(const std::string & line, std::size_t & i)
{
    if (i >= line.size()) {
        return Op::ERR;
    }
    Op op = Op::ERR;
    switch (line[i++]) {
        case '+': op = Op::ADD; break;
        case '-': op = Op::SUB; break;
        case '*': op = Op::MUL; break;
        case '/': op = Op::DIV; break;
        case '%': op = Op::REM; break;
        case '^': op = Op::POW; break;
        case 'L':
            if (i + 2 <= line.size() && line[i] == 'O' && line[i + 1] == 'G') {
                i += 2;
                op = Op::LOG;
            } else {
                return Op::ERR;
            }
            break;
        default:
            return Op::ERR;
    }
    if (i >= line.size() || line[i] != ')') {
        return Op::ERR;
    }
    ++i;
    return op;
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
        case Op::FAC:   //добавлена логика факториала и обработка ошибок
            if (current < 0 || std::floor(current) != current) {
                std::cerr << "Bad argument for factorial: " << current << std::endl;
                return current;
            }
            else {
                double result = 1;
                for (double i = 2; i <= current; ++i) {
                    result *= i;
                }
                return result;
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
        case Op::LOG:   //добавлена логика логарифма и обрабокта ошибок
            if (left <= 0) {
                std::cerr << "Bad argument for logarithm: " << left << std::endl;
                return left;
            }
            else if (right <= 0 || right == 1) {
                std::cerr << "Bad base for logarithm: " << right << std::endl;
                return left;
            }
            return std::log(left) / std::log(right);
        default:
            return left;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = skip_ws(line, 0);
    if (i < line.size() && line[i] == '(') {
        ++i;
        const Op fold_op = parse_fold_op(line, i);
        if (fold_op == Op::ERR || arity(fold_op) != 2) {
            std::cerr << "Invalid fold operation" << std::endl;
            return current;
        }
        i = skip_ws(line, i);
        double acc = current;
        while (i < line.size()) {
            const auto old_i = i;
            const double arg = parse_one_number(line, i);
            if (i == old_i) {
                std::cerr << "Expected number in fold at " << i << std::endl;
                return current;
            }
            acc = binary(fold_op, acc, arg);
            i = skip_ws(line, i);
        }
        return acc;
    }

    i = 0;
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