
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class finish_good_rep_popup : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "finish_goods_psting.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            
            if (aUsers == "external")
            {
                //Image14.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'finish_goods_psting.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox1.Text = dr["field1"].ToString();
                    TextBox2.Text = dr["field2"].ToString();
                    TextBox3.Text = dr["field3"].ToString();
                    TextBox4.Text = dr["field4"].ToString();
                    TextBox5.Text = dr["field5"].ToString();
                    TextBox6.Text = dr["field6"].ToString();
                    TextBox7.Text = dr["field7"].ToString();
                    TextBox8.Text = dr["field8"].ToString();
                    TextBox9.Text = dr["field9"].ToString();
                    TextBox10.Text = dr["field10"].ToString();
                    TextBox11.Text = dr["field11"].ToString();
                    TextBox12.Text = dr["field12"].ToString();
                    TextBox13.Text = dr["field13"].ToString();
                    TextBox14.Text = dr["field14"].ToString();
                   
                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;

                    if (dr["chk_field3"].ToString() == "True")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;

                    if (dr["chk_field4"].ToString() == "True")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false;

                    if (dr["chk_field5"].ToString() == "True")
                        CheckBox5.Checked = true;
                    else
                        CheckBox5.Checked = false;
                    if (dr["chk_field6"].ToString() == "True")
                        CheckBox9.Checked = true;
                    else
                        CheckBox9.Checked = false;

                    if (dr["chk_field7"].ToString() == "True")
                        CheckBox10.Checked = true;
                    else
                        CheckBox10.Checked = false;

                    if (dr["chk_field8"].ToString() == "True")
                        CheckBox11.Checked = true;
                    else
                        CheckBox11.Checked = false;

                    if (dr["chk_field9"].ToString() == "True")
                        CheckBox12.Checked = true;
                    else
                        CheckBox12.Checked = false;

                    


                    if (dr["rd_field1"].ToString() == "UOM")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Job#")
                        RadioButtonList1.SelectedIndex = 1;                    

                    if (dr["rd_field2"].ToString() == "Cost")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "Sell Value")
                        RadioButtonList2.SelectedIndex = 1;

                    if (dr["rd_field3"].ToString() == "FG Item#")
                        RadioButtonList3.SelectedIndex = 0;
                    if (dr["rd_field3"].ToString() == "Customer Part#")
                        RadioButtonList3.SelectedIndex = 1;

                    if (dr["rd_field4"].ToString() == "Item Name")
                        RadioButtonList4.SelectedIndex = 0;
                    if (dr["rd_field4"].ToString() == "P.O. #/Vendor")
                        RadioButtonList4.SelectedIndex = 1;                              
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

                TextBox14.Text = System.DateTime.Today.ToShortDateString();
                RadioButtonList_out.SelectedIndex = 0;                
            }
            catch { }
                        
            
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
        if (HiddenFieldPost.Value == "Yes")
        {
            if (CheckBox1.Checked)
            {
                HiddenField1.Value = "Yes";
            }
            else
            {
                HiddenField1.Value = "No";
            }
            if (CheckBox2.Checked)
            {
                HiddenField2.Value = "Yes";
            }
            else
            {
                HiddenField2.Value = "No";
            }
            if (CheckBox3.Checked)
            {
                HiddenField3.Value = "Yes";
            }
            else
            {
                HiddenField3.Value = "No";
            }
            if (CheckBox4.Checked)
            {
                HiddenField4.Value = "Yes";
            }
            else
            {
                HiddenField4.Value = "No";
            } if (CheckBox5.Checked)
            {
                HiddenField5.Value = "Yes";
            }
            else
            {
                HiddenField5.Value = "No";
            } if (CheckBox11.Checked)
            {
                HiddenField6.Value = "Yes";
            }
            else
            {
                HiddenField6.Value = "No";
            } if (CheckBox9.Checked)
            {
                HiddenField11.Value = "Yes";
            }
            else
            {
                HiddenField11.Value = "No";
            }
            if (CheckBox10.Checked)
            {
                HiddenField12.Value = "Yes";
            }
            else
            {
                HiddenField12.Value = "No";
            }
            if (CheckBox12.Checked)
            {
                HiddenField13.Value = "Yes";
            }
            else
            {
                HiddenField13.Value = "No";
            }


            if (RadioButtonList1.SelectedIndex == 0)
                HiddenField10.Value = "1";
            if (RadioButtonList1.SelectedIndex == 1)
                HiddenField10.Value = "2";

            if (RadioButtonList2.SelectedIndex == 0)
                HiddenField7.Value = "C";
            if (RadioButtonList2.SelectedIndex == 1)
                HiddenField7.Value = "S";

            if (RadioButtonList3.SelectedIndex == 0)
                HiddenField8.Value = "1";
            if (RadioButtonList3.SelectedIndex == 1)
                HiddenField8.Value = "2";

            if (RadioButtonList4.SelectedIndex == 0)
                HiddenField9.Value = "1";
            if (RadioButtonList4.SelectedIndex == 1)
                HiddenField9.Value = "2";


            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmfgpost"].DefaultValue = "FinishGPost";
            ObjectDataSource1.SelectParameters["prmBeginSeq"].DefaultValue = TextBox1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndSeq"].DefaultValue = TextBox2.Text.Trim(); ;
            ObjectDataSource1.SelectParameters["prmBeginUsrid"].DefaultValue = TextBox3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndUsrid"].DefaultValue = TextBox4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBeDate"].DefaultValue = TextBox5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = TextBox6.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBeItem"].DefaultValue = TextBox9.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox10.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBeJob"].DefaultValue = TextBox7.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndJob"].DefaultValue = TextBox8.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBeWare"].DefaultValue = TextBox11.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndWare"].DefaultValue = TextBox12.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPstDate"].DefaultValue = TextBox14.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRecept"].DefaultValue = HiddenField1.Value;
            ObjectDataSource1.SelectParameters["prmShipmnt"].DefaultValue = HiddenField2.Value;
            ObjectDataSource1.SelectParameters["prmTrnsfr"].DefaultValue = HiddenField3.Value;
            ObjectDataSource1.SelectParameters["prmAdjstmnt"].DefaultValue = HiddenField4.Value;
            ObjectDataSource1.SelectParameters["prmCrdRtn"].DefaultValue = HiddenField5.Value;
            ObjectDataSource1.SelectParameters["prmItmcod"].DefaultValue = HiddenField6.Value;
            ObjectDataSource1.SelectParameters["prmcostsell"].DefaultValue = HiddenField7.Value;
            ObjectDataSource1.SelectParameters["prmitmcustp"].DefaultValue = HiddenField8.Value;
            ObjectDataSource1.SelectParameters["prmNamPoVn"].DefaultValue = HiddenField9.Value;
            ObjectDataSource1.SelectParameters["prmUomJob"].DefaultValue = HiddenField10.Value;
            ObjectDataSource1.SelectParameters["prmGlActNm"].DefaultValue = HiddenField11.Value;
            ObjectDataSource1.SelectParameters["prmTcost"].DefaultValue = HiddenField12.Value;
            ObjectDataSource1.SelectParameters["prmGrndTotl"].DefaultValue = HiddenField13.Value;
            ObjectDataSource1.SelectParameters["prmtrnstype"].DefaultValue = TextBox13.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;
            ObjectDataSource1.SelectParameters["prmsetup"].DefaultValue = HiddenFieldPost.Value;



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'finish_goods_psting.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12,field13, field14, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6, chk_field7, chk_field8, chk_field9, rd_field1, rd_field2, rd_field3, rd_field4) values ('" + UserLogin.UserName + "','finish_goods_psting.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + TextBox8.Text.Trim() + "','" + TextBox9.Text.Trim() + "','" + TextBox10.Text.Trim() + "','" + TextBox11.Text.Trim() + "','" + TextBox12.Text.Trim() + "','" + TextBox13.Text.Trim() + "','" + TextBox14.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "','" + CheckBox3.Checked + "','" + CheckBox4.Checked + "','" + CheckBox5.Checked + "','" + CheckBox9.Checked + "','" + CheckBox10.Checked + "','" + CheckBox11.Checked + "','" + CheckBox12.Checked + "','" + RadioButtonList1.SelectedValue + "','" + RadioButtonList2.SelectedValue + "','" + RadioButtonList3.SelectedValue + "','" + RadioButtonList4.SelectedValue + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', field6 = '" + TextBox6.Text.Trim() + "', field7 = '" + TextBox7.Text.Trim() + "', field8 = '" + TextBox8.Text.Trim() + "', field9 = '" + TextBox9.Text.Trim() + "', field10 = '" + TextBox10.Text.Trim() + "', field11 = '" + TextBox11.Text.Trim() + "', field12 = '" + TextBox12.Text.Trim() + "', field13 = '" + TextBox13.Text.Trim() + "', field14 = '" + TextBox14.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "', chk_field3 = '" + CheckBox3.Checked + "', chk_field4 = '" + CheckBox4.Checked + "', chk_field5 = '" + CheckBox5.Checked + "', chk_field6 = '" + CheckBox9.Checked + "', chk_field7 = '" + CheckBox10.Checked + "', chk_field8 = '" + CheckBox11.Checked + "', chk_field9 = '" + CheckBox12.Checked + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "', rd_field3= '" + RadioButtonList3.SelectedValue + "', rd_field4 = '" + RadioButtonList4.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'finish_goods_psting.aspx' ", conn);
                    cmd_update.ExecuteNonQuery();
                }
                conn.Close();


            }
            catch (Exception ex)
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
                Label vpath = (Label)FormView1.FindControl("vfgpostLabel");
                HyperLink1.Text = vpath.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


                if (vpath.Text == "")
                {
                    OutPutFile.Text = "No CSV Exists";
                    Response.Write("<script>window.location.href='finish_goods_psting.aspx';</script>");
                }
            }
            catch { }

            Response.Write("<script>window.opener.location.reload();</script>");
        }
    }
    
}

