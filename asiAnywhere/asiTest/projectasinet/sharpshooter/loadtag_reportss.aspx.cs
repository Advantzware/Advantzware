#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.IO;
using System.Web;
#endregion


namespace projectasinet.sharpshooter
{
    public partial class loadtag_reportss : System.Web.UI.Page
    {

        private bool bSort = true;

        protected void Page_Load(object sender, System.EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "loadtag_report.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();

                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                labelcompany.Text = PrmComp;
                Session["Customers_Company"] = PrmComp;
                if (aUsers == "external")
                {

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = '../login.aspx';</script>");

                }
            }

            string sCulture = ConfigurationManager.AppSettings["LCID"];
            if (!String.IsNullOrEmpty(sCulture))
            {
                int nCulture = int.Parse(sCulture);
                System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
            }

            if (!Page.IsPostBack)
            {
                OutPutFile.Visible = false;
                HyperLink1.Visible = false;

                if (Session["User"] != null)
                {
                    lblUser.Text = UserLogin.UserName;
                }


                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();

                    string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'loadtag_report.aspx' ";
                    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                    DataSet ds = new DataSet();
                    da.Fill(ds);

                    foreach (DataRow dr in ds.Tables[0].Rows)
                    {
                        fromdate_TextBox.Text = dr["field1"].ToString();
                        todate_TextBox.Text = dr["field2"].ToString();
                        Labelpath_TextBox.Text = dr["field3"].ToString();
                        textpath_TextBox.Text = dr["field4"].ToString();
                        labelpallet_TextBox.Text = dr["field5"].ToString();
                        printerfrom_TextBox.Text = dr["field6"].ToString();

                        if (dr["chk_field1"].ToString() == "True")
                            CheckBox2.Checked = true;
                        else
                            CheckBox2.Checked = false;

                        CheckBox3.Checked = false;

                        if (dr["chk_field3"].ToString() == "True")
                            CheckBox4.Checked = true;
                        else
                            CheckBox4.Checked = false;
                        if (dr["chk_field4"].ToString() == "True")
                            CheckBox5.Checked = true;
                        else
                            CheckBox5.Checked = false;
                        if (dr["chk_field5"].ToString() == "True")
                            CheckBox6.Checked = true;
                        else
                            CheckBox6.Checked = false;
                        if (dr["chk_field6"].ToString() == "True")
                            CheckBox7.Checked = true;
                        else
                            CheckBox7.Checked = false;
                        if (dr["chk_field7"].ToString() == "True")
                            CheckBox8.Checked = true;
                        else
                            CheckBox8.Checked = false;
                        if (dr["chk_field8"].ToString() == "True")
                            CheckBox9.Checked = true;
                        else
                            CheckBox9.Checked = false;
                        if (dr["chk_field9"].ToString() == "True")
                            CheckBox10.Checked = true;
                        else
                            CheckBox10.Checked = false;
                        if (dr["chk_field10"].ToString() == "True")
                            CheckBox11.Checked = true;
                        else
                            CheckBox11.Checked = false;

                        if (dr["rd_field1"].ToString() == "Order")
                        {
                            RD1.Checked = true;
                            fromjobpoLabel.Text = "From Order#";
                            tojobpoLabel.Text = "From Order#";
                            commaLabel.Text = " Enter Order(s) separated by Comma";
                        }
                        if (dr["rd_field1"].ToString() == "PO")
                        {
                            RD2.Checked = true;
                            fromjobpoLabel.Text = "From Po#";
                            tojobpoLabel.Text = "From Po#";
                            commaLabel.Text = " Enter Po(s) separated by Comma";
                        }

                        if (dr["rd_field2"].ToString() == "O")
                            RadioButtonList2.SelectedIndex = 0;
                        if (dr["rd_field2"].ToString() == "C")
                            RadioButtonList2.SelectedIndex = 1;
                        if (dr["rd_field2"].ToString() == "A")
                            RadioButtonList2.SelectedIndex = 2;
                        if (dr["rd_field2"].ToString() == "")
                            RadioButtonList2.SelectedIndex = 2;

                        if (dr["rd_field3"].ToString() == "H")
                            RadioButtonList3.SelectedIndex = 0;
                        if (dr["rd_field3"].ToString() == "L")
                            RadioButtonList3.SelectedIndex = 1;
                        if (dr["rd_field3"].ToString() == "R")
                            RadioButtonList3.SelectedIndex = 2;
                        if (dr["rd_field3"].ToString() == "")
                            RadioButtonList3.SelectedIndex = 0;

                        if (dr["rd_field4"].ToString() == "A")
                            RadioButtonList4.SelectedIndex = 0;
                        if (dr["rd_field4"].ToString() == "U")
                            RadioButtonList4.SelectedIndex = 1;
                        if (dr["rd_field4"].ToString() == "B")
                            RadioButtonList4.SelectedIndex = 2;
                        if (dr["rd_field4"].ToString() == "")
                            RadioButtonList4.SelectedIndex = 2;
                    }

                    conn.Close();
                }
                catch
                {
                    conn.Close();
                }
                finally
                {
                    conn.Close();
                }


                try
                {


                    if (fromdate_TextBox.Text == "")
                        fromdate_TextBox.Text = "01/01/2000";
                    if (todate_TextBox.Text == "")
                        todate_TextBox.Text = "12/31/2019";
                }
                catch { }

            }
        }

        protected void hlnkLogOut_Click(object sender, EventArgs e)
        {

            string sLoginURL = ConfigurationManager.AppSettings["LoginFileSs"];
            if (sLoginURL == "")
            {
                Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
                return;
            }

            Page.Session.Clear();
            Response.Redirect(sLoginURL);
        }

        protected void Back_tomenu_Click(object sender, EventArgs e)
        {
            Response.Redirect("menu.aspx");
        }

        protected void open_popup()
        {
            string tanvalue = "";
            if (ScanLabelTextBox.Text != "")
                tanvalue = ScanLabelTextBox.Text;
            else
                tanvalue = fromOrder_TextBox.Text + "tagbalnk";
            RegisterStartupScript("str1", "<script>window.open('loadtaglookss.aspx?tagno=" + tanvalue.Trim() + "','ItemWindow','width=860,height=520, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");
        }


        protected void submitbutton_click(object sender, EventArgs e)
        {
            string jobpurc = "";
            if (RD1.Checked == true)
                jobpurc = "Order";
            if (RD2.Checked == true)
                jobpurc = "PO";
            if (fromOrder_TextBox.Text == "")
                fromOrder_TextBox.Text = "0";
            if (torder_TextBox.Text == "")
                torder_TextBox.Text = "0";
            if (fromjob2_TextBox.Text == "")
                fromjob2_TextBox.Text = "0";
            if (tojob2_TextBox.Text == "")
                tojob2_TextBox.Text = "0";
            if (labelpallet_TextBox.Text == "")
                labelpallet_TextBox.Text = "0";
            if (printerfrom_TextBox.Text == "")
                printerfrom_TextBox.Text = "0";
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            reports load = new reports();
            DataSet dsload = new DataSet();

            if (!CheckBox3.Checked)
            {
                string path1 = Labelpath_TextBox.Text.Trim();
                string firstchar1 = path1.Substring(0, 1);
                string laststr1 = path1.Substring(1, path1.Length - 1);
                if (firstchar1 == "p" || firstchar1 == "P")
                {
                    path1 = "D" + laststr1;
                }
                if (firstchar1 == "q" || firstchar1 == "Q")
                {
                    path1 = "E" + laststr1;
                }

                if (!System.IO.File.Exists(path1))
                {
                    HttpContext.Current.Response.Write("<script>alert('Label Matrix Label file path is not valid')</script>");
                    return;
                }

                dsload = load.SharpShooterLoadTag(UserLogin.UserName, "ldtag", ordercomma_TextBox.Text, jobcomm_TextBox.Text, Convert.ToInt32(fromOrder_TextBox.Text), Convert.ToInt32(torder_TextBox.Text), fromjob_TextBox.Text, Convert.ToInt32(fromjob2_TextBox.Text), tojob_TextBox.Text, Convert.ToInt32(tojob2_TextBox.Text), fromitem_TextBox.Text, toitem_TextBox.Text, Convert.ToString(jobpurc), Convert.ToString(CheckBox2.Checked), Convert.ToString(CheckBox3.Checked), ScanLabelTextBox.Text, Convert.ToString(RadioButtonList2.SelectedValue), Convert.ToString(RadioButtonList3.SelectedValue), fromdate_TextBox.Text, todate_TextBox.Text, Convert.ToString(RadioButtonList4.SelectedValue), Convert.ToString(CheckBox4.Checked), Convert.ToString(CheckBox8.Checked), Convert.ToString(CheckBox5.Checked), Convert.ToString(CheckBox9.Checked), Convert.ToString(CheckBox6.Checked), Convert.ToString(CheckBox7.Checked), Convert.ToString(CheckBox10.Checked), Convert.ToString(CheckBox11.Checked), Labelpath_TextBox.Text, Convert.ToInt32(labelpallet_TextBox.Text), Convert.ToInt32(printerfrom_TextBox.Text), textpath_TextBox.Text, deptnotes_TextBox.Text, shipnotes_TextBox.Text, "First", "", "No", "", "");
                string scantagvalue = "";
                if (ScanLabelTextBox.Text != "")
                    scantagvalue = ScanLabelTextBox.Text;
                else
                {
                    scantagvalue = fromOrder_TextBox.Text + "tagbalnk";
                }

                try
                {
                    SqlCommand cmd_delete = new SqlCommand("delete from temp_table where temp20 = '" + scantagvalue.Trim() + "' ", conn);
                    cmd_delete.ExecuteNonQuery();

                    for (int j = 0; j < dsload.Tables[0].Rows.Count; j++)
                    {
                        SqlCommand cmd_insert = new SqlCommand("insert into temp_table(file_name, temp1 , temp2, temp3, temp4, temp5, temp6, temp7,temp8, temp9, temp10,temp11,temp12,temp13,temp14,temp15,temp16,temp17,temp18,temp19,temp20) values ('" + UserLogin.UserName + "','" + dsload.Tables[0].Rows[j][2].ToString() + "' , '" + dsload.Tables[0].Rows[j][1].ToString() + "', '" + dsload.Tables[0].Rows[j][3].ToString() + "', '" + dsload.Tables[0].Rows[j][4].ToString() + "', '" + dsload.Tables[0].Rows[j][5].ToString() + "', '" + dsload.Tables[0].Rows[j][6].ToString() + "', '" + dsload.Tables[0].Rows[j][7].ToString() + "', '" + dsload.Tables[0].Rows[j][8].ToString() + "', '" + dsload.Tables[0].Rows[j][9].ToString() + "', '" + dsload.Tables[0].Rows[j][10].ToString() + "','" + dsload.Tables[0].Rows[j][11].ToString() + "','" + dsload.Tables[0].Rows[j][12].ToString() + "','" + dsload.Tables[0].Rows[j][13].ToString() + "','" + dsload.Tables[0].Rows[j][14].ToString() + "','" + dsload.Tables[0].Rows[j][15].ToString() + "','" + dsload.Tables[0].Rows[j][16].ToString() + "','" + dsload.Tables[0].Rows[j][17].ToString() + "','" + dsload.Tables[0].Rows[j][0].ToString() + "','" + dsload.Tables[0].Rows[j][18].ToString() + "','" + scantagvalue.Trim() + "')", conn);
                        cmd_insert.ExecuteNonQuery();

                    }
                }
                catch { }
            }
            else
            {
                if (HiddenFieldPost.Value == "Yes")
                {
                    string path1 = Labelpath_TextBox.Text.Trim();
                    string firstchar1 = path1.Substring(0, 1);
                    string laststr1 = path1.Substring(1, path1.Length - 1);
                    if (firstchar1 == "p" || firstchar1 == "P")
                    {
                        path1 = "D" + laststr1;
                    }
                    if (firstchar1 == "q" || firstchar1 == "Q")
                    {
                        path1 = "E" + laststr1;
                    }
                    if (!System.IO.File.Exists(path1))
                    {
                        HttpContext.Current.Response.Write("<script>alert('Label Matrix Label file path is not valid')</script>");
                        return;
                    }

                    dsload = load.SharpShooterLoadTag(UserLogin.UserName, "ldtag", ordercomma_TextBox.Text, jobcomm_TextBox.Text, Convert.ToInt32(fromOrder_TextBox.Text), Convert.ToInt32(torder_TextBox.Text), fromjob_TextBox.Text, Convert.ToInt32(fromjob2_TextBox.Text), tojob_TextBox.Text, Convert.ToInt32(tojob2_TextBox.Text), fromitem_TextBox.Text, toitem_TextBox.Text, Convert.ToString(jobpurc), Convert.ToString(CheckBox2.Checked), Convert.ToString(CheckBox3.Checked), ScanLabelTextBox.Text, Convert.ToString(RadioButtonList2.SelectedValue), Convert.ToString(RadioButtonList3.SelectedValue), fromdate_TextBox.Text, todate_TextBox.Text, Convert.ToString(RadioButtonList4.SelectedValue), Convert.ToString(CheckBox4.Checked), Convert.ToString(CheckBox8.Checked), Convert.ToString(CheckBox5.Checked), Convert.ToString(CheckBox9.Checked), Convert.ToString(CheckBox6.Checked), Convert.ToString(CheckBox7.Checked), Convert.ToString(CheckBox10.Checked), Convert.ToString(CheckBox11.Checked), Labelpath_TextBox.Text, Convert.ToInt32(labelpallet_TextBox.Text), Convert.ToInt32(printerfrom_TextBox.Text), textpath_TextBox.Text, deptnotes_TextBox.Text, shipnotes_TextBox.Text, "First", "", "No", "", "");


                    if (CheckBox10.Checked)
                    {
                        string str1 = Labelpath_TextBox.Text.Trim();

                        string firstchar = str1.Substring(0, 1);
                        string laststr = str1.Substring(1, str1.Length - 1);
                        if (firstchar == "p" || firstchar == "P")
                        {
                            str1 = "D" + laststr;
                        }
                        if (firstchar == "q" || firstchar == "Q")
                        {
                            str1 = "E" + laststr;
                        }


                        string str2 = Path.GetFileName(str1);

                        string path = str2;
                        if (!Request.Browser.Browser.Contains("Safari"))
                        {

                            string fileName = str2;
                            System.IO.FileStream fs = null;
                            fs = System.IO.File.Open(str1, System.IO.FileMode.Open); ;
                            byte[] btFile = new byte[fs.Length];
                            fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                            fs.Close();
                            Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                            Response.ContentType = "application/octet-stream";
                            Response.BinaryWrite(btFile);
                            Response.End();
                            fs = null;
                        }

                    }
                }

            }


            if (!CheckBox3.Checked)
            {
                open_popup();
            }




            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'loadtag_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            try
            {
                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4,field5,field6,rd_field1,rd_field2,rd_field3,rd_field4,chk_field1, chk_field2, chk_field3,chk_field4,chk_field5,chk_field6,chk_field7,chk_field8,chk_field9,chk_field10) values ('" + UserLogin.UserName + "','loadtag_report.aspx','" + fromdate_TextBox.Text.Trim() + "','" + todate_TextBox.Text.Trim() + "','" + Labelpath_TextBox.Text.Trim() + "','" + textpath_TextBox.Text.Trim() + "','" + labelpallet_TextBox.Text.Trim() + "','" + printerfrom_TextBox.Text.Trim() + "','" + jobpurc + "','" + RadioButtonList2.SelectedValue + "','" + RadioButtonList3.SelectedValue + "','" + RadioButtonList4.SelectedValue + "','" + Convert.ToString(CheckBox2.Checked) + "','" + Convert.ToString(CheckBox3.Checked) + "','" + Convert.ToString(CheckBox4.Checked) + "','" + Convert.ToString(CheckBox5.Checked) + "','" + Convert.ToString(CheckBox6.Checked) + "','" + Convert.ToString(CheckBox7.Checked) + "','" + Convert.ToString(CheckBox8.Checked) + "','" + Convert.ToString(CheckBox9.Checked) + "','" + Convert.ToString(CheckBox10.Checked) + "','" + Convert.ToString(CheckBox11.Checked) + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + fromdate_TextBox.Text.Trim() + "', field2 = '" + todate_TextBox.Text.Trim() + "', field3 = '" + Labelpath_TextBox.Text.Trim() + "', field4 = '" + textpath_TextBox.Text.Trim() + "', field5 = '" + labelpallet_TextBox.Text.Trim() + "', field6 = '" + printerfrom_TextBox.Text.Trim() + "', rd_field1 = '" + jobpurc + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "', rd_field3 = '" + RadioButtonList3.SelectedValue + "', rd_field4 = '" + RadioButtonList4.SelectedValue + "',chk_field1 = '" + Convert.ToString(CheckBox2.Checked) + "',chk_field2 = '" + Convert.ToString(CheckBox3.Checked) + "', chk_field3 = '" + Convert.ToString(CheckBox4.Checked) + "', chk_field4 = '" + Convert.ToString(CheckBox5.Checked) + "', chk_field5 = '" + Convert.ToString(CheckBox6.Checked) + "',chk_field6 = '" + Convert.ToString(CheckBox7.Checked) + "',chk_field7 = '" + Convert.ToString(CheckBox8.Checked) + "',chk_field8 = '" + Convert.ToString(CheckBox9.Checked) + "',chk_field9 = '" + Convert.ToString(CheckBox10.Checked) + "',chk_field10 = '" + Convert.ToString(CheckBox11.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='loadtag_report.aspx' ", conn);
                    cmd_update.ExecuteNonQuery();
                }
            }
            catch { }

            conn.Close();

        }

        protected void createtagbutton_click(object sender, EventArgs e)
        {
            string jobpurc = "";
            if (RD1.Checked == true)
                jobpurc = "Order";
            if (RD2.Checked == true)
                jobpurc = "PO";
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            string filepath = Server.MapPath("MyCSVFolder") + "\\" + UserLogin.UserName + "loadtag.txt";

            FormView1.ChangeMode(FormViewMode.ReadOnly);

            reports load = new reports();
            DataSet dsload = new DataSet();
            dsload = load.SharpShooterLoadTag(UserLogin.UserName, "ldtag", ordercomma_TextBox.Text, jobcomm_TextBox.Text, Convert.ToInt32(fromOrder_TextBox.Text), Convert.ToInt32(torder_TextBox.Text), fromjob_TextBox.Text, Convert.ToInt32(fromjob2_TextBox.Text), tojob_TextBox.Text, Convert.ToInt32(tojob2_TextBox.Text), fromitem_TextBox.Text, toitem_TextBox.Text, Convert.ToString(jobpurc), Convert.ToString(CheckBox2.Checked), Convert.ToString(CheckBox3.Checked), ScanLabelTextBox.Text, Convert.ToString(RadioButtonList2.SelectedValue), Convert.ToString(RadioButtonList3.SelectedValue), fromdate_TextBox.Text, todate_TextBox.Text, Convert.ToString(RadioButtonList4.SelectedValue), Convert.ToString(CheckBox4.Checked), Convert.ToString(CheckBox8.Checked), Convert.ToString(CheckBox5.Checked), Convert.ToString(CheckBox9.Checked), Convert.ToString(CheckBox6.Checked), Convert.ToString(CheckBox7.Checked), Convert.ToString(CheckBox10.Checked), Convert.ToString(CheckBox11.Checked), Labelpath_TextBox.Text, Convert.ToInt32(labelpallet_TextBox.Text), Convert.ToInt32(printerfrom_TextBox.Text), textpath_TextBox.Text, deptnotes_TextBox.Text, shipnotes_TextBox.Text, "Second", "", filepath, "", "");

            if (CheckBox10.Checked)
            {
                string str1 = Labelpath_TextBox.Text.Trim();

                string firstchar = str1.Substring(0, 1);
                string laststr = str1.Substring(1, str1.Length - 1);
                if (firstchar == "p" || firstchar == "P")
                {
                    str1 = "D" + laststr;
                }
                if (firstchar == "Q" || firstchar == "q")
                {
                    str1 = "E" + laststr;
                }
                string str2 = Path.GetFileName(str1);

                string path = str2;
                if (!Request.Browser.Browser.Contains("Safari"))
                {

                    string fileName = str2;
                    System.IO.FileStream fs = null;
                    fs = System.IO.File.Open(str1, System.IO.FileMode.Open); ;
                    byte[] btFile = new byte[fs.Length];
                    fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                    fs.Close();
                    Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                    Response.ContentType = "application/octet-stream";
                    Response.BinaryWrite(btFile);
                    Response.End();
                    fs = null;
                }
            }



        }


    }
}