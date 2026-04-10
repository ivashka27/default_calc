#include "calc.hpp" // Подключаем заголовочный файл
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <iomanip>

// --- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ---

// Функция удаления пробелов по краям
static std::string trim(const std::string &str) {
    size_t first = str.find_first_not_of(' ');
    if (std::string::npos == first) return "";
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
}

// Получение числового значения символа (0-9, A-F)
static int digit_value(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'F') return 10 + c - 'A';
    if (c >= 'a' && c <= 'f') return 10 + c - 'a';
    return -1;
}

// Проверка лимита в 10 цифр (условие задания)
static bool check_digit_limit(const std::string &s) {
    int count = 0;
    for (char c : s) {
        if (digit_value(c) != -1) count++;
    }
    return count <= 10;
}

// Универсальный парсер чисел (Decimal, Binary, Octal, Hex)
// Реализует задание для группы 25Б82
static double parse_number(const std::string &original_s) {
    std::string s = trim(original_s);
    if (s.empty()) throw std::invalid_argument("Empty number");

    if (!check_digit_limit(s)) {
        throw std::invalid_argument("Number exceeds 10 digit limit");
    }

    std::string s_upper = s;
    std::transform(s_upper.begin(), s_upper.end(), s_upper.begin(), ::toupper);

    int base = 10;
    size_t start_index = 0;

    // Определяем систему счисления по префиксу
    if (s_upper.rfind("0B", 0) == 0) { // Двоичная
        base = 2;
        start_index = 2;
    } else if (s_upper.rfind("0X", 0) == 0) { // Шестнадцатеричная
        base = 16;
        start_index = 2;
    } else if (s.length() > 1 && s[0] == '0') { // Восьмеричная (начинается с 0)
        // Проверяем, что это действительно восьмеричное (цифры 0-7)
        bool is_octal = true;
        for(size_t i=1; i<s.length(); ++i) {
            if (s[i] == '.') continue;
            int val = digit_value(s[i]);
            if (val < 0 || val >= 8) is_octal = false; 
        }
        if (is_octal) {
            base = 8;
            start_index = 1; // Пропускаем первый ноль
        }
    }

    // Если десятичная система, используем стандартную функцию
    if (base == 10) {
        try {
            return std::stod(s);
        } catch (...) {
            throw std::invalid_argument("Invalid decimal number");
        }
    }

    // Парсинг для систем с основанием 2, 8, 16
    std::string num_part = s_upper.substr(start_index);
    
    size_t dot_pos = num_part.find('.');
    std::string int_part_str = (dot_pos == std::string::npos) ? num_part : num_part.substr(0, dot_pos);
    std::string frac_part_str = (dot_pos == std::string::npos) ? "" : num_part.substr(dot_pos + 1);

    double result = 0.0;

    // Обработка целой части
    if (!int_part_str.empty()) {
        for (char c : int_part_str) {
            int val = digit_value(c);
            if (val < 0 || val >= base) throw std::invalid_argument("Invalid digit for base");
            result = result * base + val;
        }
    }

    // Обработка дробной части
    if (!frac_part_str.empty()) {
        double weight = 1.0 / base;
        for (char c : frac_part_str) {
            int val = digit_value(c);
            if (val < 0 || val >= base) throw std::invalid_argument("Invalid digit for base");
            result += val * weight;
            weight /= base;
        }
    }

    return result;
}

// Выполнение бинарной операции
static double apply_binary_op(char op, double left, double right) {
    switch (op) {
        case '+': return left + right;
        case '-': return left - right;
        case '*': return left * right;
        case '/': 
            if (right == 0) throw std::runtime_error("Division by zero");
            return left / right;
        case '%': 
            if (right == 0) throw std::runtime_error("Modulo by zero");
            return std::fmod(left, right);
        case '^': return std::pow(left, right);
        default: throw std::runtime_error("Unknown binary operator");
    }
}

// --- ОСНОВНАЯ ФУНКЦИЯ ИНТЕРФЕЙСА ---

double process_line(double reg, const std::string &line) {
    std::string trimmed = trim(line);
    if (trimmed.empty()) return reg;

    std::stringstream ss(trimmed);
    std::string token;
    ss >> token;

    // 1. Бонусное задание: Свёртка (Fold)
    // Синтаксис: (+) 1 2 3
    if (token.size() >= 2 && token[0] == '(' && token.back() == ')') {
        char op = token[1];
        std::string valid_ops = "+-*/%^";
        
        // Проверяем, что оператор валидный
        if (valid_ops.find(op) == std::string::npos) {
            std::cerr << "Error: Invalid fold operator '" << op << "'" << std::endl;
            return reg;
        }

        double current_val = reg; // Начальное значение - регистр
        std::string arg_str;
        bool has_args = false;
        
        while (ss >> arg_str) {
            has_args = true;
            try {
                double arg = parse_number(arg_str);
                current_val = apply_binary_op(op, current_val, arg);
            } catch (const std::exception& e) {
                std::cerr << "Error: " << e.what() << std::endl;
                return reg; // При ошибке сохраняем текущее состояние
            }
        }
        return current_val;
    }

    // 2. Унарные операции
    if (token == "_") return -reg; // Инверсия знака
    
    if (token == "SQRT") {
        if (reg < 0) {
            std::cerr << "Error: Square root of negative number" << std::endl;
            return std::nan(""); // Возвращаем NaN
        }
        return std::sqrt(reg);
    }

    // 3. Бинарные операции
    std::string binary_ops = "+-*/%^";
    if (binary_ops.find(token) != std::string::npos) {
        char op = token[0];
        std::string arg_str;
        
        if (!(ss >> arg_str)) {
            std::cerr << "Error: Missing argument for operation " << op << std::endl;
            return reg;
        }
        
        try {
            double arg = parse_number(arg_str);
            return apply_binary_op(op, reg, arg);
        } catch (const std::exception& e) {
            std::cerr << "Error: " << e.what() << std::endl;
            return reg;
        }
    }

    // Если дошли сюда, команда не распознана
    std::cerr << "Error: Unknown command '" << token << "'" << std::endl;
    return reg;
}
