
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
using System.Web.UI;
#endregion

public partial class order_backl_report_list : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "order_backl_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
           
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    OutPutFile.Visible = false;
                    HyperLink1.Visible = false;

                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        TextBox1.Text = aDefaultCust;
                        TextBox2.Text = aDefaultCust;
                    }
                    catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_backl_report.aspx' ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);

                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                            
                            besmanTextBox.Text = dr["field3"].ToString();
                            endsmanTextBox.Text = dr["field4"].ToString();
                            beorTextBox.Text = dr["field5"].ToString();
                            endordTextBox.Text = dr["field6"].ToString();
                            beitemTextBox.Text = dr["field7"].ToString();
                            enditTextBox.Text = dr["field8"].ToString();
                            bedateTextBox.Text = dr["field9"].ToString();
                            enddateTextBox.Text = dr["field10"].ToString();
                            beuserTextBox.Text = dr["field11"].ToString();
                            enduserTextBox.Text = dr["field12"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field5"].ToString() == "Yes")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;

                            if (dr["rd_field1"].ToString() == "Yes")
                                RadioButtonList1.SelectedIndex = 0;
                            else
                                RadioButtonList1.SelectedIndex = 1;

                            if (dr["rd_field2"].ToString() == "C")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "D")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "S")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "P")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "S")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "PO")
                                RadioButtonList3.SelectedIndex = 2;

                            if (dr["rd_field4"].ToString() == "Yes")
                                RadioButtonList4.SelectedIndex = 0;
                            else
                                RadioButtonList4.SelectedIndex = 1;

                            if (dr["rd_field5"].ToString() == "Yes")
                                RadioButtonList5.SelectedIndex = 0;
                            else
                                RadioButtonList5.SelectedIndex = 1;
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
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);
                    try
                    {
                        Image4.Visible = false;
                        //Label begin = (Label)FormView2.FindControl("CustLabel");
                        //TextBox1.Text = begin.Text;
                        //TextBox2.Text = begin.Text;
                        TextBox2.ReadOnly = true;
                    }
                    catch { }
                    
                    //RadioButtonList1.SelectedIndex = 0;
                    //CheckBox1.Checked = true;
                    //CheckBox2.Checked = true;
                    //RadioButtonList2.SelectedIndex = 0;
                    //RadioButtonList3.SelectedIndex = 1;
                    //RadioButtonList4.SelectedIndex = 0;
                    //RadioButtonList5.SelectedIndex = 0;
                    if(endsmanTextBox.Text == "")
                    endsmanTextBox.Text = "zzz";
                    if(bedateTextBox.Text == "")
                    bedateTextBox.Text = "01/01/2005";
                    if(enddateTextBox.Text == "")
                    enddateTextBox.Text = "12/31/2007";
                    if(endordTextBox.Text == "")
                    endordTextBox.Text = "99999999";
                    if(enditTextBox.Text == "")
                    enditTextBox.Text = "zzzzzzzzzzzzzzz";
                    if(enduserTextBox.Text == "")
                    enduserTextBox.Text = "zzzzzzzz";
                    if (RadioButtonList_out.SelectedIndex == -1)
                    {
                        RadioButtonList_out.SelectedIndex = 0;
                    }
                    if (Session["User"] != null)
                    {
                        lblUser.Text = UserLogin.UserName;
                    }
                }

            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    OutPutFile.Visible = false;
                    HyperLink1.Visible = false;

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_backl_report.aspx' ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);

                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            //Response.Write(dr["begin_order"].ToString());
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            besmanTextBox.Text = dr["field3"].ToString();
                            endsmanTextBox.Text = dr["field4"].ToString();
                            beorTextBox.Text = dr["field5"].ToString();
                            endordTextBox.Text = dr["field6"].ToString();
                            beitemTextBox.Text = dr["field7"].ToString();
                            enditTextBox.Text = dr["field8"].ToString();
                            bedateTextBox.Text = dr["field9"].ToString();
                            enddateTextBox.Text = dr["field10"].ToString();
                            beuserTextBox.Text = dr["field11"].ToString();
                            enduserTextBox.Text = dr["field12"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox3.Checked = true;
                            else
                                CheckBox3.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field5"].ToString() == "Yes")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;

                            if (dr["rd_field1"].ToString() == "Yes")
                                RadioButtonList1.SelectedIndex = 0;
                            else
                                RadioButtonList1.SelectedIndex = 1;

                            if (dr["rd_field2"].ToString() == "C")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "D")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "S")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "P")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "S")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "PO")
                                RadioButtonList3.SelectedIndex = 2;

                            if (dr["rd_field4"].ToString() == "Yes")
                                RadioButtonList4.SelectedIndex = 0;
                            else
                                RadioButtonList4.SelectedIndex = 1;

                            if (dr["rd_field5"].ToString() == "Yes")
                                RadioButtonList5.SelectedIndex = 0;
                            else
                                RadioButtonList5.SelectedIndex = 1;
                        }

                        if (endsmanTextBox.Text == "")
                            endsmanTextBox.Text = "zzz";
                        if (bedateTextBox.Text == "")
                            bedateTextBox.Text = "01/01/2005";
                        if (enddateTextBox.Text == "")
                            enddateTextBox.Text = "12/31/2007";
                        if (endordTextBox.Text == "")
                            endordTextBox.Text = "99999999";
                        if (enditTextBox.Text == "")
                            enditTextBox.Text = "zzzzzzzzzzzzzzz";
                        if (enduserTextBox.Text == "")
                            enduserTextBox.Text = "zzzzzzzz";
                        if (RadioButtonList_out.SelectedIndex == -1)
                        {
                            RadioButtonList_out.SelectedIndex = 0;
                        }

                        
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }

                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);
                    
                    if (Session["User"] != null)
                    {
                        lblUser.Text = UserLogin.UserName;
                    }
                }

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
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

    protected void submitbutton_click(object sender, EventArgs e)
    {


        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "Yes";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "NO";

        if (CheckBox1.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        }
        if (CheckBox3.Checked)
        {
            HiddenField4.Value = "Yes";
        }
        else
        {
            HiddenField4.Value = "No";
        }
        if (CheckBox4.Checked)
        {
            HiddenField5.Value = "Yes";
        }
        else
        {
            HiddenField5.Value = "No";
        }
        if (CheckBox5.Checked)
        {
            HiddenField6.Value = "Yes";
        }
        else
        {
            HiddenField6.Value = "No";
        }
        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField7.Value = "C";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField7.Value = "D";
        if (RadioButtonList2.SelectedIndex == 2)
            HiddenField7.Value = "S";

        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField8.Value = "P";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField8.Value = "S";
        if (RadioButtonList3.SelectedIndex == 2)
            HiddenField8.Value = "PO";

        if (RadioButtonList4.SelectedIndex == 0)
            HiddenField9.Value = "Yes";
        if (RadioButtonList4.SelectedIndex == 1)
            HiddenField9.Value = "NO";

        if (RadioButtonList5.SelectedIndex == 0)
            HiddenField10.Value = "Yes";
        if (RadioButtonList5.SelectedIndex == 1)
            HiddenField10.Value = "NO";


        UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "order_backl_report.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();

                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                try
                {
                    if (aUsers == "internal")
                    {
                        
                        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                        ObjectDataSource1.SelectParameters["prmAct"].DefaultValue = "printlog";
                        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBegsman"].DefaultValue = besmanTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndsman"].DefaultValue = endsmanTextBox.Text.Trim();

                        ObjectDataSource1.SelectParameters["prmBegord"].DefaultValue = beorTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndord"].DefaultValue = endordTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBeitem"].DefaultValue = beitemTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEnditem"].DefaultValue = enditTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBegdate"].DefaultValue = bedateTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = enddateTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBeuser"].DefaultValue = beuserTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEnduser"].DefaultValue = enduserTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmPrint"].DefaultValue = HiddenField2.Value;
                        ObjectDataSource1.SelectParameters["prminjob"].DefaultValue = HiddenField3.Value;
                        ObjectDataSource1.SelectParameters["prmDet"].DefaultValue = HiddenField4.Value;
                        ObjectDataSource1.SelectParameters["prmInIt"].DefaultValue = HiddenField5.Value;
                        ObjectDataSource1.SelectParameters["prmSub"].DefaultValue = HiddenField6.Value;
                        ObjectDataSource1.SelectParameters["prmReLa"].DefaultValue = HiddenField1.Value;
                        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField7.Value;
                        ObjectDataSource1.SelectParameters["prmPrip"].DefaultValue = HiddenField8.Value;
                        ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = HiddenField9.Value;
                        ObjectDataSource1.SelectParameters["prmDis"].DefaultValue = HiddenField10.Value;
                        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;
                    }
                }
                catch { };

                if (aUsers == "external")
                {
                    
                    try
                    {
                        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                        ObjectDataSource1.SelectParameters["prmAct"].DefaultValue = "printlog";
                        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBegsman"].DefaultValue = besmanTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndsman"].DefaultValue = endsmanTextBox.Text.Trim();

                        ObjectDataSource1.SelectParameters["prmBegord"].DefaultValue = beorTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndord"].DefaultValue = endordTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBeitem"].DefaultValue = beitemTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEnditem"].DefaultValue = enditTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBegdate"].DefaultValue = bedateTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = enddateTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmBeuser"].DefaultValue = beuserTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmEnduser"].DefaultValue = enduserTextBox.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmPrint"].DefaultValue = HiddenField2.Value;
                        ObjectDataSource1.SelectParameters["prminjob"].DefaultValue = HiddenField3.Value;
                        ObjectDataSource1.SelectParameters["prmDet"].DefaultValue = HiddenField4.Value;
                        ObjectDataSource1.SelectParameters["prmInIt"].DefaultValue = HiddenField5.Value;
                        ObjectDataSource1.SelectParameters["prmSub"].DefaultValue = HiddenField6.Value;
                        ObjectDataSource1.SelectParameters["prmReLa"].DefaultValue = HiddenField1.Value;
                        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField7.Value;
                        ObjectDataSource1.SelectParameters["prmPrip"].DefaultValue = HiddenField8.Value;
                        ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = HiddenField9.Value;
                        ObjectDataSource1.SelectParameters["prmDis"].DefaultValue = HiddenField10.Value;
                        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;
                    }
                    catch { }
                }
            }

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_backl_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);
                //Response.Write(ds.Tables[0].TableName);
                //Response.Write(ds.Tables[0].Rows.Count);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, rd_field1, rd_field2, rd_field3, rd_field4, rd_field5) values ('" + UserLogin.UserName + "','order_backl_report.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox2.Text.Trim() + "','" + besmanTextBox.Text.Trim() + "','" + endsmanTextBox.Text.Trim() + "','" + beorTextBox.Text.Trim() + "','" + endordTextBox.Text.Trim() + "','" + beitemTextBox.Text.Trim() + "','" + enditTextBox.Text.Trim() + "','" + bedateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "','" + beuserTextBox.Text.Trim() + "','" + enduserTextBox.Text.Trim() + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + HiddenField1.Value + "','" + HiddenField7.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + besmanTextBox.Text.Trim() + "', field4 = '" + endsmanTextBox.Text.Trim() + "', field5 = '" + beorTextBox.Text.Trim() + "', field6 = '" + endordTextBox.Text.Trim() + "', field7 = '" + beitemTextBox.Text.Trim() + "', field8 = '" + enditTextBox.Text.Trim() + "', field9 = '" + bedateTextBox.Text.Trim() + "', field10 = '" + enddateTextBox.Text.Trim() + "', field11 = '" + beuserTextBox.Text.Trim() + "', field12 = '" + enduserTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField2.Value + "', chk_field2 = '" + HiddenField3.Value + "', chk_field3 = '" + HiddenField4.Value + "', chk_field4 = '" + HiddenField5.Value + "', chk_field5 = '" + HiddenField6.Value + "', rd_field1 = '" + HiddenField1.Value + "', rd_field2 = '" + HiddenField7.Value + "', rd_field3= '" + HiddenField8.Value + "', rd_field4 = '" + HiddenField9.Value + "', rd_field5 = '" + HiddenField10.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'order_backl_report.aspx' ", conn);
                    cmd_update.ExecuteNonQuery();
                }
                conn.Close();
            }
            catch(Exception ex)
            {
                Label1.Text = "Error :" + ex.Message + "<p>";
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {

                OutPutFile.Visible = true;
                HyperLink1.Visible = true;
                Label vpath = (Label)FormView1.FindControl("backFileLabel");
                HyperLink1.Text = vpath.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;
                //HyperLink1.NavigateUrl = @"O:/wwwroot/pdfs/backlog20110505.txt" ;
                //HyperLink1.NavigateUrl = "download.aspx";
                //?file='" + vpath.Text + "'
                //DirectoryInfo di = new DirectoryInfo(Server.MapPath("~/Images/arrow down.jpge"));
                //int i = 0;
                //foreach (FileInfo fi in di.GetFiles())
                //{
                //    HyperLink HL = new HyperLink();
                //    HL.ID = "HyperLink" + i++;
                //    HL.Text = fi.Name;
                //    HL.NavigateUrl = "download.aspx?file=" + fi.Name;
                //    Page.Controls.Add(HL);
                //    Page.Controls.Add(new LiteralControl("<br/>"));
                //} 


                if (vpath.Text == "")
                {
                    OutPutFile.Text = "No CSV Exists";
                    Response.Write("<script>window.location.href='order_backl_report.aspx'</script>");
                }
            }
            catch { }

            
    }

       

}
