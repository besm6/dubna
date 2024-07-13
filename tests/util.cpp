//
// Utility routines for unit tests.
//
// Copyright (c) 2023 Serge Vakulenko
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
#include "util.h"

#include <fstream>
#include <cstring>

#include "fixture_machine.h"
#include "fixture_session.h"

//
// Get current test name, as specified in TEST() macro.
//
std::string get_test_name()
{
    std::string name = ::testing::UnitTest::GetInstance()->current_test_info()->name();

    // Remove the '/1' suffix.
    auto pos = name.find_last_of('/');
    if (pos != std::string::npos)
        name.erase(pos);

    return name;
}

//
// Read file contents and return it as a string.
//
std::string file_contents(const std::string &filename)
{
    std::ifstream input(filename);
    if (!input.is_open()) {
        std::cerr << filename << ": " << std::strerror(errno) << std::endl;
        return "";
    }
    std::stringstream contents;
    contents << input.rdbuf();
    return contents.str();
}

//
// Read file contents as vector of strings.
//
std::vector<std::string> split_stream(std::istream &input)
{
    std::vector<std::string> output;
    std::string line;
    while (std::getline(input, line)) {
        output.push_back(line);
    }
    return output;
}

//
// Read file contents as vector of strings.
//
std::vector<std::string> file_contents_split(const std::string &filename)
{
    std::ifstream input(filename);
    return split_stream(input);
}

//
// Read file contents as vector of strings.
//
std::vector<std::string> multiline_split(const std::string &multiline)
{
    std::stringstream input(multiline);
    return split_stream(input);
}

//
// Read FILE* stream contents until EOF and return it as a string.
//
std::string stream_contents(FILE *input)
{
    std::stringstream contents;
    char line[256];
    while (fgets(line, sizeof(line), input)) {
        contents << line;
    }
    return contents.str();
}

//
// Create file with given contents.
//
void create_file(const std::string &filename, const std::string &contents)
{
    std::ofstream output(filename);
    output << contents;
}

//
// Create file with given contents.
//
void create_file(const std::string &dest_filename, const std::string &prolog,
                 const std::string &src_filename, const std::string &epilog)
{
    std::ofstream output(dest_filename);
    EXPECT_TRUE(output.is_open()) << dest_filename;

    std::ifstream input(src_filename);
    EXPECT_TRUE(input.is_open()) << src_filename;

    output << prolog;
    output << input.rdbuf();
    output << epilog;
}

//
// Check whether string starts with given prefix.
//
bool starts_with(const std::string &str, const char *prefix)
{
    auto prefix_size = strlen(prefix);
    return str.size() >= prefix_size && memcmp(str.c_str(), prefix, prefix_size) == 0;
}
