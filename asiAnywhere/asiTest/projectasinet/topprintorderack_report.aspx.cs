
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

public partial class topprintorderack_report : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "topprintorderack_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["print_ack_order"] = PrmComp;
            if (aUsers == "external")
            {
               if (!Page.IsPostBack)
                {

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topprintorderack_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            beginorder_TextBox.Text = dr["field1"].ToString();
                            endorder_TextBox.Text = dr["field1"].ToString();
                            begincust_TextBox.Text = dr["field3"].ToString();
                            endcust_TextBox.Text = dr["field4"].ToString();
                            txt_begin_date.Text = dr["field5"].ToString();
                            txt_end_date.Text = dr["field6"].ToString();
                            txt_begin_rel.Text = dr["field7"].ToString();
                            txt_end_rel.Text = dr["field8"].ToString();
                           
                            //if (dr["rd_field1"].ToString() == "3")
                            //    rdl_month.SelectedIndex = 0;
                            //if (dr["rd_field1"].ToString() == "6")
                            //    rdl_month.SelectedIndex = 1;
                           

                            
                            if (dr["chk_field1"].ToString() == "Yes")
                                chk_reprint_ack.Checked = true;
                            else
                                chk_reprint_ack.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                chk_cons_form.Checked = true;
                            else
                                chk_cons_form.Checked = false;
                            //if (dr["chk_field3"].ToString() == "Yes")
                            //    chk_ware_months.Checked = true;
                            //else
                            //    chk_ware_months.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                chk_sch_rel.Checked = true;
                            else
                                chk_sch_rel.Checked = false;
                            if (dr["chk_field5"].ToString() == "Yes")
                                chk_spec_notes.Checked = true;
                            else
                                chk_spec_notes.Checked = false;

                            if (dr["chk_field6"].ToString() == "Yes")
                                chk_ship.Checked = true;
                            else
                                chk_ship.Checked = false;
                            if (dr["chk_field7"].ToString() == "Yes")
                                chk_act_rel.Checked = true;
                            else
                                chk_act_rel.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                chk_revised.Checked = true;
                            else
                                chk_revised.Checked = false;
                            if (dr["chk_field9"].ToString() == "Yes")
                                chk_bom.Checked = true;
                            else
                                chk_bom.Checked = false;                           

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
                    endcust_TextBox.ReadOnly = true;
                    Image3.Visible = false;
                    if(txt_end_rel.Text == "")
                    txt_end_rel.Text = "99999999";
                   if(txt_begin_date.Text == "")
                    txt_begin_date.Text = DateTime.Now.ToShortDateString();
                   if(txt_end_date.Text == "")
                    txt_end_date.Text = DateTime.Now.ToShortDateString();
                   

                }
            
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topprintorderack_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            beginorder_TextBox.Text = dr["field1"].ToString();
                            endorder_TextBox.Text = dr["field2"].ToString();
                            begincust_TextBox.Text = dr["field3"].ToString();
                            endcust_TextBox.Text = dr["field4"].ToString();
                            txt_begin_date.Text = dr["field5"].ToString();
                            txt_end_date.Text = dr["field6"].ToString();
                            txt_begin_rel.Text = dr["field7"].ToString();
                            txt_end_rel.Text = dr["field8"].ToString();

                            //if (dr["rd_field1"].ToString() == "3")
                            //    rdl_month.SelectedIndex = 0;
                            //if (dr["rd_field1"].ToString() == "6")
                            //    rdl_month.SelectedIndex = 1;



                            if (dr["chk_field1"].ToString() == "Yes")
                                chk_reprint_ack.Checked = true;
                            else
                                chk_reprint_ack.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                chk_cons_form.Checked = true;
                            else
                                chk_cons_form.Checked = false;
                            //if (dr["chk_field3"].ToString() == "Yes")
                            //    chk_ware_months.Checked = true;
                            //else
                            //    chk_ware_months.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                chk_sch_rel.Checked = true;
                            else
                                chk_sch_rel.Checked = false;
                            if (dr["chk_field5"].ToString() == "Yes")
                                chk_spec_notes.Checked = true;
                            else
                                chk_spec_notes.Checked = false;

                            if (dr["chk_field6"].ToString() == "Yes")
                                chk_ship.Checked = true;
                            else
                                chk_ship.Checked = false;
                            if (dr["chk_field7"].ToString() == "Yes")
                                chk_act_rel.Checked = true;
                            else
                                chk_act_rel.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                chk_revised.Checked = true;
                            else
                                chk_revised.Checked = false;
                            if (dr["chk_field9"].ToString() == "Yes")
                                chk_bom.Checked = true;
                            else
                                chk_bom.Checked = false;

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
                    
                    if (txt_end_rel.Text == "")
                        txt_end_rel.Text = "99999999";
                    if (txt_begin_date.Text == "")
                        txt_begin_date.Text = DateTime.Now.ToShortDateString();
                    if (txt_end_date.Text == "")
                        txt_end_date.Text = DateTime.Now.ToShortDateString();

                    

                }


                beginorder_TextBox.Text = Convert.ToString(Session["order"]);
                endorder_TextBox.Text = Convert.ToString(Session["order"]);
                begincust_TextBox.Text = Convert.ToString(Session["cust"]);
                endcust_TextBox.Text = Convert.ToString(Session["cust"]);

            

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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (chk_act_rel.Checked)
        {
            hid_act_rel.Value = "Yes";
        }
        
        if (chk_bom.Checked)
        {
            hid_bom.Value = "Yes";
        }
       
        if (chk_cons_form.Checked)
        {
            hid_cons_frm.Value = "Yes";
        }
        
        if (chk_reprint_ack.Checked)
            hid_reprint.Value = "Yes";
        if (chk_revised.Checked)
            hid_revise.Value = "Yes";
        if (chk_sch_rel.Checked)
            hid_sch_rel.Value = "Yes";
        if (chk_spec_notes.Checked)
            hid_whs_months.Value = "Yes";
        if (chk_ware_months.Checked)
            hid_whs_months.Value = "Yes";
        if (chk_ship.Checked)
            hid_ship_to.Value = "Yes";
        if (rdl_month.SelectedIndex == 0)
            hid_months.Value = "3";
        if (rdl_month.SelectedIndex == 1)
            hid_months.Value = "6";
        //Response.Write("hello");
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "OrderAck";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = "No";
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = endcust_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginOrder"].DefaultValue = beginorder_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndOrder"].DefaultValue = endorder_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginOrdDate"].DefaultValue = txt_begin_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndOrdDate"].DefaultValue = txt_end_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginRel"].DefaultValue = txt_begin_rel.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndRel"].DefaultValue = txt_end_rel.Text.Trim();
        ObjectDataSource1.SelectParameters["prmReprintAck"].DefaultValue = hid_reprint.Value;
        ObjectDataSource1.SelectParameters["prmConsForm"].DefaultValue = hid_cons_frm.Value;
        //ObjectDataSource1.SelectParameters["prmWareHouse"].DefaultValue = hid_whs_months.Value;
        //ObjectDataSource1.SelectParameters["prmMonths"].DefaultValue = hid_months.Value;
        ObjectDataSource1.SelectParameters["prmSchRel"].DefaultValue = hid_sch_rel.Value;
        ObjectDataSource1.SelectParameters["prmSpecNotes"].DefaultValue = hid_tb_inst.Value;
        ObjectDataSource1.SelectParameters["prmShipAddr"].DefaultValue = hid_ship_to.Value;
        ObjectDataSource1.SelectParameters["prmActRel"].DefaultValue = hid_act_rel.Value;
        ObjectDataSource1.SelectParameters["prmPrintRevised"].DefaultValue = hid_revise.Value;
        ObjectDataSource1.SelectParameters["prmBillMat"].DefaultValue = hid_bom.Value;



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topprintorderack_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8,rd_field1, chk_field1, chk_field2,chk_field3,chk_field4,chk_field5,chk_field6,chk_field7,chk_field8,chk_field9) values ('" + UserLogin.UserName + "','topprintorderack_report.aspx','" + beginorder_TextBox.Text.Trim() + "','" + endorder_TextBox.Text.Trim() + "','" + begincust_TextBox.Text.Trim() + "','" + endcust_TextBox.Text.Trim() + "','" + txt_begin_date.Text.Trim() + "','" + txt_end_date.Text.Trim() + "','" + txt_begin_rel.Text.Trim() + "','" + txt_end_rel.Text.Trim() + "','" + hid_months.Value + "','" + hid_reprint.Value + "','" + hid_cons_frm.Value + "','" + hid_whs_months.Value + "','" + hid_sch_rel.Value + "','" + hid_whs_months.Value + "','" + hid_ship_to.Value + "','" + hid_act_rel.Value + "','" + hid_revise.Value + "','" + hid_bom.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + beginorder_TextBox.Text.Trim() + "', field2 = '" + endorder_TextBox.Text.Trim() + "', field3 = '" + begincust_TextBox.Text.Trim() + "', field4 = '" + endcust_TextBox.Text.Trim() + "', field5 = '" + txt_begin_date.Text.Trim() + "', field6 = '" + txt_end_date.Text.Trim() + "', field7 = '" + txt_begin_rel.Text.Trim() + "', field8 = '" + txt_end_rel.Text.Trim() + "', rd_field1 = '" + hid_months.Value + "', chk_field1 = '" + hid_reprint.Value + "', chk_field2 = '" + hid_cons_frm.Value + "',chk_field3 = '" + hid_whs_months.Value + "',chk_field4 = '" + hid_sch_rel.Value + "', chk_field5 = '" + hid_whs_months.Value + "',chk_field6 = '" + hid_ship_to.Value + "',chk_field7 = '" + hid_act_rel.Value + "',chk_field8 = '" + hid_revise.Value + "', chk_field9 = '" + hid_bom.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='topprintorderack_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

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
        Label vpath = (Label)FormView1.FindControl("vFileLabel");
        
        if (vpath.Text != "")
        {
            string path = vpath.Text;
            string path2 = @"/pdfs/" + path;
            Session["open_order_ack_rep"] = path2;           
            if (path2 != "")
            {
                if (!Request.Browser.Browser.Contains("Safari"))
                    Response.Write("<script>window.open('print_open_order_ack_rep.aspx'); target='_blank'</script>");
                else
                Response.Redirect("topprintorderack_report.aspx");
            }
        }
        else
        {
            Label1.Text = "No Pdf Exists";
        }
    }
    catch
    {
        Label1.Text = "No Pdf Exists";
    }
    if (Label1.Text == "")
    {
        Response.Write("<script>window.location.href='topprintorderack_report.aspx'</script>");
    }
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        
    }
}
