#include "calc.hpp"

#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr
#include <string>
#include <algorithm>

namespace {

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
    const auto rollback = [&i, &line] (const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };
    if (i >= line.size()) return Op::ERR;

    char c = line[i++];
    if (std::isdigit(c)) {
        --i; 
        return Op::SET;
    }

    switch (c) {
        case '+': return Op::ADD;
        case '-': return Op::SUB;
        case '*': return Op::MUL;
        case '/': return Op::DIV;
        case '%': return Op::REM;
        case '_': return Op::NEG;
        case '^': return Op::POW;
        case 'S':
            if (line.substr(i, 3) == "QRT") {
                i += 3;
                return Op::SQRT;
            }
            return rollback(1);
        default:
            return rollback(1);
    }
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(static_cast<unsigned char>(line[i]))) {
        ++i;
    }
    return i;
}

// Вспомогательная функция для конвертации символа в число
int char_to_int(char c) {
    c = std::tolower(static_cast<unsigned char>(c));
    if (std::isdigit(c)) return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    return -1;
}

double parse_arg(const std::string & line, std::size_t & i)
{
    double res = 0;
    int base = 10;

    // Определение системы счисления
    if (i + 2 < line.size() && line[i] == '0') {
        char prefix = std::tolower(static_cast<unsigned char>(line[i+1]));
        if (prefix == 'b') {
            base = 2;
            i += 2;
        } else if (prefix == 'x') {
            base = 16;
            i += 2;
        } else if (std::isdigit(line[i+1]) || line[i+1] == '.') {
            base = 8;
            i += 1;
        }
    } else if (i + 1 < line.size() && line[i] == '0' && line[i+1] != '.') {
        base = 8;
        i += 1;
    }

    
    while (i < line.size()) {
        int val = char_to_int(line[i]);
        if (val == -1 || val >= base) break;
        res = res * base + val;
        ++i;
    }

    
    if (i < line.size() && line[i] == '.') {
        ++i;
        double weight = 1.0 / base;
        while (i < line.size()) {
            int val = char_to_int(line[i]);
            if (val == -1 || val >= base) break;
            res += val * weight;
            weight /= base;
            ++i;
        }
    }

    return res;
}

double unary(const double current, const Op op)
{
    switch (op) {
        case Op::NEG: return -current;
        case Op::SQRT:
            if (current >= 0) return std::sqrt(current);
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            return current;
        default: return current;
    }
}

double binary(const Op op, const double left, const double right)
{
    switch (op) {
        case Op::SET: return right;
        case Op::ADD: return left + right;
        case Op::SUB: return left - right;
        case Op::MUL: return left * right;
        case Op::DIV:
            if (right != 0) return left / right;
            std::cerr << "Bad right argument for division" << std::endl;
            return left;
        case Op::REM:
            if (right != 0) return std::fmod(left, right);
            return left;
        case Op::POW: return std::pow(left, right);
        default: return left;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    i = skip_ws(line, i);
    const auto op = parse_op(line, i);
    
    switch (arity(op)) {
        case 2: {
            i = skip_ws(line, i);
            const auto old_i = i;
            const auto arg = parse_arg(line, i);
            i = skip_ws(line, i);
            if (i == old_i && op != Op::SET) {
                std::cerr << "No argument for binary operation" << std::endl;
                break;
            }
            return binary(op, current, arg);
        }
        case 1: {
            i = skip_ws(line, i);
            if (i < line.size()) {
                std::cerr << "Unexpected suffix" << std::endl;
            }
            return unary(current, op);
        }
        default: break;
    }
    return current;
}
