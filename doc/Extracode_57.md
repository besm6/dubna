# Процедуры заказа дисковых ресурсов

Обращение через *57 77777.

На сумматоре при входе:

 * разряды 48-30, 24-21 содержат ключ доступа
 * разряды 15-1  - адрес информационного поля
 * разряды 20-16 - код операции
 * разряды 29-25 - дополнительная информация

Коды операций:

 * 0в - заказ пакетов
 * 1в - отказ от пакетов
 * 2в - запрос координат файлов
 * 3в - заказ обычных файлов
 * 4в - заказ scratch файлов
 * 5в - отказ от файлов
 * 6в - отказ от всех мд-ресурсов
 * 37в - изменение статуса файла

Пример:
           дополнительная информация
           /\
    0000 0240 4005 3416
    \______| / \/\____/
           |/   |  адрес информационного поля
          ключ  код
        доступа операции
