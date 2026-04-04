#include "../include/calc.hpp" //исправлен путь до папки

#include <iostream>
#include <string>

int main()
{
    double current = 0;
    for (std::string line; std::getline(std::cin, line); ) {
        current = process_line(current, line);
        std::cout << current << std::endl;
    }
}