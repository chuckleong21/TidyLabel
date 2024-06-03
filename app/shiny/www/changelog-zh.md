---
output: html_document
---

<head>
  <script src="https://kit.fontawesome.com/f817ba0cef.js" crossorigin="anonymous"></script>
</head>


## 版本说明

自版本0.7.0以后，因为调用MS Word等程式，只能在Windows上运行。

### 1.5.0

  - <i class="fa-regular fa-circle" style="color:steelblue;"></i> 改进应用设置页面的UI

### 1.4.2
  
  - <i class="fas fa-question" style="color:red;"></i> 新增字符串相似度算法选择（目前使用[Jaro–Winkler](https://blog.csdn.net/a553181867/article/details/89057661)距离）

### 1.4.0

- **税费整理**
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增2个版本清关单识别方法
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 清关单现需要输入识别页面范围参数
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增清关单版本匹配状态显示
- **标签对照**
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增标签对照
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 使用`{tidystringdist}`API计算箱单和标签的字符串相似度
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增箱单表格标准化方法
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增显示HTML表格的API`{gt}`
- **应用设置**
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增清关单版本预设选项：`自动`、`7805640197`、 `2540263576`、 `7801664214`、 `7816168667`
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增标签对照开关
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 新增箱单文件选项（只能上传后缀名为`xlsx`的文件）
- 其他
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 更换读取Excel文件速度更快的API`{openxlsx2}`
  - <i class="fa-regular fa-circle-check" style="color:forestgreen;"></i> 翻译UI

### 1.1.0 

- <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> ~~新增`2024-05`版本的税费整理方法，保留`2023-12`版本的税费整理方法~~
- <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 配合多版本的税费整理新增应用设置页面
  - 整合语言设置选项
  - 新增税费整理版本的选择
- <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 更新部分API
- <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 更新UI关键字的语种互译

### 1.0.1

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 小型错误修复

### 1.0.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 增加英文版本

### 0.9.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **税费整理**增加税费编码及其小项
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 增加**税费整理**汇总选项 & 根据税费编码进行筛选税费
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **标签整理**新增标签图片迁移
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **标签整理**新增预览选项
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 改进UI
-   <i class="fas fa-question" style="color:red;"></i> ~~增加文件缓存~~

### 0.8.1

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **税费整理**减少重复计算
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **税费整理**改用俄文表头

### 0.8.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **税费整理**可直接上传并处理PDF格式文件
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **税费整理**不再使用Excel的方法
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **税费整理**增加重命名表头功能

### 0.7.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> **标签整理**新增`套用百世格式`选项

### 0.6.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 封装功能至`Shiny`网络应用

### 0.1.0

-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 新增**标签整理**方法
-   <i class="fa-regular fa-circle-check" style="color:forestgreen"></i> 新增**税费整理**方法
